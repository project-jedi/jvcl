unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, StdActns, ActnList, Menus, ExtCtrls, ComCtrls, JvStatusBar,
  JvComCtrls, JvSurveyIntf, JvDialogs, StdCtrls, JvListView;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    JvStatusBar1: TJvStatusBar;
    tvItems: TJvTreeView;
    nbDetails: TNotebook;
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
    acPrinterSettings: TAction;
    acPrint: TAction;
    N5: TMenuItem;
    PrinterSettings1: TMenuItem;
    Print1: TMenuItem;
    N6: TMenuItem;
    OpenSurveyDialog: TJvOpenDialog;
    lvGlobalStats: TListView;
    reFreeForm: TRichEdit;
    lvItemStats: TJvListView;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrintDialog: TPrintDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acOpenExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure tvItemsCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure acAddResponseExecute(Sender: TObject);
    procedure acPrinterSettingsExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
  private
    { Private declarations }
    FFilename: string;
    FResponses: TStringlist;
    FSurvey: IJvSurvey;
    procedure LoadView;
    procedure LoadData(Node: TTreeNode);
    function GetResponseValue(item:IJvSurveyItem;Index:integer):integer;
    function AddItem(Parent: TTreeNode; item: IJvSurveyItem): TTreeNode;
    procedure AddResponses(item: IJvSurveyItem; Index: integer);
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromResponse(const Filename: string);
    property Filename: string read FFilename write FFilename;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation
uses
  JvSurveyUtils, JclStrings, Math;

{$R *.DFM}

const
  cSurveyItemImageIndex = 22;

procedure TfrmMain.LoadFromFile(const Filename: string);
var i:integer;
begin
  FResponses.Clear;
  FSurvey.LoadFromFile(Filename);
  self.Filename := Filename;
  // clear any responses added to survey by mistake
  for i := 0 to FSurvey.Items.Count - 1 do
    FSurvey.Items[i].Responses := '';
  LoadView;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FSurvey := CreateSurvey;
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
    LoadFromFile(OpenSurveyDialog.Filename);
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

function TfrmMain.GetResponseValue(item: IJvSurveyItem;
  Index: integer): integer;
var S:TStringlist;
begin
  Result := 0;
  if item.SurveyType = stFreeForm then Exit;
  S := TStringlist.Create;
  try
    S.Text := DecodeResponse(item.Responses,item.SurveyType);
    if (Index < 0) or (Index >= S.Count) then Exit;
    Result := StrToIntDef(S[Index],0);
  finally
    S.Free;
  end;
end;

procedure TfrmMain.LoadData(Node: TTreeNode);
var item:IJvSurveyItem;li:TListItem;S,T:TStringlist;i:integer;
begin
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
        S.Text := DecodeChoice(item.Choices,item.SurveyType);
        for i := 0 to S.Count - 1 do
        begin
          if S[i] = '' then Continue;
          li := lvItemStats.Items.Add;
          li.Caption := S[i];
          li.SubItems.Add(IntToStr(GetResponseValue(item,i)));
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

procedure TfrmMain.tvItemsChange(Sender: TObject; Node: TTreeNode);
begin
  LoadData(Node);
end;

procedure TfrmMain.acAddResponseExecute(Sender: TObject);
begin
  OpenSurveyDialog.Filter := SResponseFileFilter;
  OpenSurveyDialog.Filename := Filename;
  if OpenSurveyDialog.Execute then
  begin
    LoadFromResponse(OpenSurveyDialog.Filename);
    LoadData(tvItems.Selected);
  end;
end;

procedure TfrmMain.LoadFromResponse(const Filename: string);
var
  ASurvey: IJvSurvey;
  i: integer;
begin
  ASurvey := CreateSurvey;
  ASurvey.LoadFromFile(Filename);
  if ASurvey.ID <> FSurvey.ID then
    raise Exception.Create('Response file is not compatible with the currently loaded survey: please select another response file.');
  if FResponses.IndexOf(ASurvey.SurveyTaker.ID) > -1 then
    raise Exception.Create('Responses from this user have already been added to this report.');
  FResponses.Add(ASurvey.SurveyTaker.ID);
  for i := 0 to ASurvey.Items.Count - 1 do
    AddResponses(ASurvey.Items[i], i);
end;

procedure TfrmMain.AddResponses(item: IJvSurveyItem; Index: integer);

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
    S1.Count := Max(S1.Count,S2.Count);
    S2.Count := Max(S1.Count,S2.Count);
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
    FSurvey.Items[Index].Responses := trim(FSurvey.Items[Index].Responses) +  '\n===========================\n' + item.Responses
  else
    MergeResponses(FSurvey.Items[Index], item);
end;

procedure TfrmMain.acPrinterSettingsExecute(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TfrmMain.acPrintExecute(Sender: TObject);
begin
  if PrintDialog.Execute then
    ShowMessage('TODO: printing not implemented yet!');
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  MessageBox(GetFocus,PChar('JEDI Surveyor Reporter, version 1.0'),PChar('About...'),MB_OK or MB_ICONINFORMATION); 
end;

end.

