unit main_donators;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, AppEvnts;

type
  TfrmGenDonate = class(TForm)
    btnScan: TButton;
    btnWrite: TButton;
    lvAuthors: TListView;
    pmChangeAuthor: TPopupMenu;
    miEditSortName: TMenuItem;
    miIgnoreAuthor: TMenuItem;
    edAuthor: TEdit;
    AppEvents: TApplicationEvents;
    lblInfo: TLabel;
    procedure btnScanClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure miEditSortNameClick(Sender: TObject);
    procedure miIgnoreAuthorClick(Sender: TObject);
    procedure lvAuthorsData(Sender: TObject; Item: TListItem);
    procedure AppEventsIdle(Sender: TObject; var Done: Boolean);
    procedure pmChangeAuthorPopup(Sender: TObject);
    procedure lvAuthorsChanging(Sender: TObject; Item: TListItem;
      Change: TItemChange; var AllowChange: Boolean);
    procedure edAuthorKeyPress(Sender: TObject; var Key: Char);
    procedure edAuthorExit(Sender: TObject);
  private
    { Private declarations }
    procedure AddAuthor(const AuthorName: string);
    procedure AddTranslationFor(const AuthorName: string);
    procedure ApplyEdit;
    procedure CancelEdit;
    function CreateTranslationFor(const AuthorName: string): string;
    procedure ScanFile(const FileName: string);
    procedure ScanFiles;
    procedure SetSortName(const AuthorName, SortName: string);
    procedure SortAuthorsFullName;
    procedure SortAuthorsSortName;
    procedure WriteOutAuthors;
  public
    { Public declarations }
  end;

var
  frmGenDonate: TfrmGenDonate;

implementation

var
  HasScanned: Boolean;
  TranslationList: TStrings;
  AuthorList: TStrings;
  HelpPath: string;

{$R *.DFM}

procedure InitHelpPath;
var
  I: Integer;
begin
  HelpPath := ExtractFilePath(Application.ExeName);
  I := Length(HelpPath) - 1;
  while (I > 0) and (HelpPath[I] <> '\') do
    Dec(I);
  Delete(HelpPath, I + 1, Length(HelpPath) - I);
end;

procedure ReadTranslationList;
begin
  TranslationList.LoadFromFile(HelpPath + 'tools\DonatorTranslator.lst');
end;

procedure WriteTranslationList;
begin
  TranslationList.SaveToFile(HelpPath + 'tools\DonatorTranslator.lst');
end;

procedure TfrmGenDonate.AddAuthor(const AuthorName: string);
begin
  if AuthorList.IndexOf(AuthorName) = -1 then
  begin
    AuthorList.Add(AuthorName);
    AddTranslationFor(AuthorName);
  end;
end;

procedure TfrmGenDonate.AddTranslationFor(const AuthorName: string);
begin
  if TranslationList.IndexOfName(AuthorName) = -1 then
    TranslationList.Add(AuthorName + '=' + CreateTranslationFor(AuthorName));
end;

procedure TfrmGenDonate.ApplyEdit;
begin
  SetSortName(lvAuthors.Selected.Caption, edAuthor.Text);
  CancelEdit;
end;

procedure TfrmGenDonate.CancelEdit;
begin
  edAuthor.Visible := False;
  lvAuthors.SetFocus;
end;

function TfrmGenDonate.CreateTranslationFor(const AuthorName: string): string;
var
  I: Integer;
begin
  I := Length(AuthorName);
  while (I > 0) and (AuthorName[I] <> ' ') do
    Dec(I);
  Result := Trim(Copy(AuthorName, I + 1, Length(AuthorName) - I)) + ', ' +
    Trim(Copy(AuthorName, 1, I - 1));
end;

procedure TfrmGenDonate.ScanFile(const FileName: string);
var
  SL: TStrings;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(HelpPath + FileName);
    I := 0;
    while (I < SL.Count - 1) and not AnsiSameText(Trim(SL[I]), 'Author') and not AnsiSameText(Trim(SL[I]), 'Author:') do
      Inc(I);
    if I < SL.Count - 2 then
      AddAuthor(Trim(SL[I + 1]));
  finally
    SL.Free;
  end;
end;

procedure TfrmGenDonate.ScanFiles;
var
  Res: Integer;
  SearchRec: TSearchRec;
begin
  btnScan.Enabled := False;
  btnWrite.Enabled := False;
  try
    Res := FindFirst(HelpPath + 'Jv*.dtx', faAnyFile, SearchRec);
    try
      while Res = 0 do
      begin
        lblInfo.Caption := 'File: ' + SearchRec.Name;
        if Pos('.', SearchRec.Name) = (Length(SearchRec.Name) - 3) then
        begin
          lblInfo.Caption := 'File: ' + SearchRec.Name;
          ScanFile(SearchRec.Name);
        end;
        Res := FindNext(SearchRec);
      end;
    finally
      FindClose(SearchRec);
    end;
    SortAuthorsSortName;
    lvAuthors.Items.Count := AuthorList.Count;
    lblInfo.Caption := '';
  finally
    btnWrite.Enabled := True;
    btnScan.Enabled := True;
  end;
end;

procedure TfrmGenDonate.SetSortName(const AuthorName, SortName: string);
begin
  TranslationList.Values[AuthorName] := SortName;
  lvAuthors.Refresh;
end;

procedure TfrmGenDonate.SortAuthorsFullName;
begin
  TStringList(AuthorList).Sort;
end;

function CompareAuthors(List: TStringList; Index1, Index2: Integer): Integer;
var
  Name1: string;
  Name2: string;
begin
  Name1 := List[Index1];
  Name2 := List[Index2];
  if TranslationList.IndexOfName(Name1) > -1 then
    Name1 := TranslationList.Values[Name1]
  else
    Name1 := '';
  if TranslationList.IndexOfName(Name2) > -1 then
    Name2 := TranslationList.Values[Name2]
  else
    Name2 := '';
  Result := AnsiCompareStr(Name1, Name2);
  if Result = 0 then
      Result := Index1 - Index2;
end;

procedure TfrmGenDonate.SortAuthorsSortName;
begin
  TStringList(AuthorList).CustomSort(CompareAuthors);
end;

procedure TfrmGenDonate.WriteOutAuthors;
var
  AuthorFile: TStrings;
  I: Integer;
begin
  AuthorFile := TStringList.Create;
  try
    AuthorFile.Add(StringOfChar('#', 100));
    AuthorFile.Add('## Donator list generated on ' + FormatDateTime('mm-dd-yyyy hh:nn:ss', Now) +
      StringOfChar(' ', 100 - 19 - 27 - 4) + '##');
    AuthorFile.Add(StringOfChar('#', 100));
    AuthorFile.Add('');
    for I := 0 to AuthorList.Count - 1 do
    begin
      if not AnsiSameText(TranslationList.Values[AuthorList[I]], '<ignore>') then
        AuthorFile.Add('  * ' + AuthorList[I]);
    end;
    AuthorFile.SaveToFile(HelpPath + 'generated includes\JVCL.Donators.dtx');
  finally
    AuthorFile.Free;
  end;
end;

procedure TfrmGenDonate.btnScanClick(Sender: TObject);
begin
  AuthorList.Clear;
  ScanFiles;
end;

procedure TfrmGenDonate.btnWriteClick(Sender: TObject);
begin
  SortAuthorsSortName;
  try
    WriteOutAuthors;
  finally
    SortAuthorsFullName;
  end;
end;

procedure TfrmGenDonate.miEditSortNameClick(Sender: TObject);
var
  TempRect: TRect;
begin
  TempRect := lvAuthors.Selected.DisplayRect(drBounds);
  TempRect.Left := TempRect.Left + lvAuthors.Columns[0].Width;
  edAuthor.SetBounds(TempRect.Left + lvAuthors.Left, TempRect.Top + lvAuthors.Top, TempRect.Right -
    TempRect.Left, TempRect.Bottom - TempRect.Top);
  if miIgnoreAuthor.Checked then
    edAuthor.Text := CreateTranslationFor(lvAuthors.Selected.Caption)
  else
    edAuthor.Text := lvAuthors.Selected.SubItems[0];
  edAuthor.Visible := True;
  edAuthor.SetFocus;
end;

procedure TfrmGenDonate.miIgnoreAuthorClick(Sender: TObject);
begin
  if miIgnoreAuthor.Checked then
    miEditSortName.Click
  else
    SetSortName(AuthorList[lvAuthors.Selected.Index], '<ignore>');
end;

procedure TfrmGenDonate.lvAuthorsData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := AuthorList[Item.Index];
  Item.SubItems.Add(TranslationList.Values[Item.Caption]);
end;

procedure TfrmGenDonate.AppEventsIdle(Sender: TObject; var Done: Boolean);
begin
  if not HasScanned then
  begin
    HasScanned := True;
    btnScan.Click;
  end;
end;

procedure TfrmGenDonate.pmChangeAuthorPopup(Sender: TObject);
begin
  if lvAuthors.Selected <> nil then
  begin
    miEditSortName.Enabled := True;
    miIgnoreAuthor.Enabled := True;
    miIgnoreAuthor.Checked := AnsiSameText(lvAuthors.Selected.SubItems[0], '<ignore>');
  end
  else
  begin
    miEditSortName.Enabled := False;
    miIgnoreAuthor.Enabled := False;
    miIgnoreAuthor.Checked := False;
  end;
end;

procedure TfrmGenDonate.lvAuthorsChanging(Sender: TObject; Item: TListItem;
  Change: TItemChange; var AllowChange: Boolean);
begin
  if (Change = ctState) and edAuthor.Visible then
    CancelEdit;
end;

procedure TfrmGenDonate.edAuthorKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ApplyEdit;
  end
  else if Key = #27 then
  begin
    Key := #0;
    CancelEdit;
  end;
end;

procedure TfrmGenDonate.edAuthorExit(Sender: TObject);
begin
  CancelEdit;
end;

initialization
  TranslationList := TStringList.Create;
  AuthorList := TStringList.Create;
  InitHelpPath;
  ReadTranslationList;

finalization
  AuthorList.Free;
  WriteTranslationList;
  TranslationList.Free;
end.
