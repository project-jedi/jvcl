unit UnitStatusDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MainCtrl, Settings, JvComponent, JvSearchFiles, ComCtrls,
  StdCtrls;

type
  TfrmUnitStatus = class(TForm)
    ListView1: TListView;
    JvSearchFiles1: TJvSearchFiles;
    Button1: TButton;
    procedure JvSearchFiles1FindFile(Sender: TObject; const AName: string);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FCtrl: TMainCtrl;
    FBuffer: array[0..511] of Char;
    FColumnToSort: Integer;
    FInitialized: Boolean;
    procedure Init;
    procedure AddItem(const AFileName, SecondString: string);
    procedure AddIgnoredItem(const AFileName: string);
    procedure ProcessFile(const AFileName: string);
  public
    class procedure Execute(ACtrl: TMainCtrl);
  end;

implementation

{$R *.dfm}
const
  CReview = 'Review';

  COther = 'Other';

  CCompleted = 'Completed';

  CIncomplete = 'Incomplete';

  CIgnored = 'Ignored';

  CGenerated = 'Generated';

  CLocked = 'Locked';

  { usCompleted, usIgnored, usGenerated, usOther }

  CUnitStatusStr: array[TUnitStatus] of string = (
    CCompleted, CIgnored, CGenerated, COther);

//=== Local procedures =======================================================

function StrToUnitStatus(const S: string): TUnitStatus;
var
  UnitStatus: TUnitStatus;
begin
  for UnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
    if SameText(CUnitStatusStr[UnitStatus], S) then
    begin
      Result := UnitStatus;
      Exit;
    end;

  Result := usOther;
end;

function Interpret(const S: string): string;
begin
  if S = '' then
    Result := COther
  else
    if Pos('review', S) > 0 then
    Result := CReview
  else
    if Pos('incomplete', S) > 0 then
    Result := CIncomplete
  else
    if Pos('complete', S) > 0 then
    Result := CCompleted
  else
    if Pos('generate', S) > 0 then
    Result := CGenerated
  else
    if Pos('lock', S) > 0 then
    Result := CLocked
  else
    Result := COther;
end;

//=== TfrmUnitStatus =========================================================

procedure TfrmUnitStatus.AddIgnoredItem(const AFileName: string);
begin
  with ListView1.Items.Add do
  begin
    Caption := AFileName;
    SubItems.Add('-');
    SubItems.Add(CIgnored);
  end;
end;

procedure TfrmUnitStatus.AddItem(const AFileName, SecondString: string);
begin
  with ListView1.Items.Add do
  begin
    Caption := AFileName;
    SubItems.Add(SecondString);
    SubItems.Add(Interpret(LowerCase(SecondString)));
  end;
end;

procedure TfrmUnitStatus.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  with TSettings.Instance do
    for I := 0 to ListView1.Items.Count - 1 do
      with ListView1.Items[I] do
        AddToUnitStatus(StrToUnitStatus(SubItems[1]), Caption);

  TSettings.Instance.SaveUnitStatusAll;
end;

class procedure TfrmUnitStatus.Execute(ACtrl: TMainCtrl);
begin
  with TfrmUnitStatus.Create(Application) do
  try
    FCtrl := ACtrl;
    ShowModal;
  finally
    Free;
  end;
end;

{procedure TfrmUnitStatus.UpdateUnitStatus(const AFileName: string;
  const AUnitStatus: TUnitStatus);
var
  LFileName: string;
  LUnitStatus: TUnitStatus;
  I: Integer;
begin
  LFileName := ChangeFileExt(AFileName, '.pas');
  with TSettings.Instance do
  begin
    for LUnitStatus := Low(TUnitStatus) to High(TUnitStatus) do
      if LUnitStatus = AUnitStatus then
        UnitsStatus[LUnitStatus].Add(LFileName)
      else
      begin
        I := UnitsStatus[LUnitStatus].IndexOf(LFileName);
        if I >= 0 then
          UnitsStatus[LUnitStatus].Delete(I);
      end;
  end;
end;}

procedure TfrmUnitStatus.FormActivate(Sender: TObject);
begin
  Init;
end;

procedure TfrmUnitStatus.Init;
var
  Cursor: TCursor;
begin
  if FInitialized then
    Exit;
  FInitialized := True;

  JvSearchFiles1.RootDirectory := TSettings.Instance.RealDtxDir;

  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    ListView1.Items.BeginUpdate;
    try
      JvSearchFiles1.Search;
    finally
      ListView1.Items.EndUpdate;
    end;
  finally
    Screen.Cursor := Cursor;
  end;
end;

procedure TfrmUnitStatus.JvSearchFiles1FindFile(Sender: TObject;
  const AName: string);
begin
  ProcessFile(AName);
end;

procedure TfrmUnitStatus.ListView1ColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  FColumnToSort := Column.Index;
  (Sender as TCustomListView).AlphaSort;
end;

procedure TfrmUnitStatus.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
begin
  if FColumnToSort = 0 then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else
  begin
    ix := FColumnToSort - 1;
    Compare := CompareText(Item1.SubItems[ix], Item2.SubItems[ix]);
  end;
end;

procedure TfrmUnitStatus.ProcessFile(const AFileName: string);
const
  CSearchString = '##Status';
var
  Stream: TFileStream;
  P, Q, R: PChar;
  BytesRead: Integer;
  S: string;
  Found: Boolean;
  LExtractFileName: string;
begin
  LExtractFileName := ExtractFileName(AFileName);

  if TSettings.Instance.IsUnitFrom(usIgnored, LExtractFileName) then
  begin
    AddIgnoredItem(LExtractFileName);
    Exit;
  end;

  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    BytesRead := Stream.read(FBuffer, SizeOf(FBuffer));
    P := FBuffer;
    if BytesRead < SizeOf(FBuffer) then
      Q := FBuffer + BytesRead - 1
    else
      Q := FBuffer + SizeOf(FBuffer) - 1;

    Found := False;
    while (P < Q - Length(CSearchString)) and not Found do
    begin
      Found := StrLIComp(P, CSearchString, Length(CSearchString)) = 0;
      Inc(P, Length(CSearchString));
      if not Found then
      begin
        while (P < Q) and (P^ <> #13) do
          Inc(P);
        while (P < Q) and (P^ in [#10, #13]) do
          Inc(P);
      end;
    end;
    S := '';
    if Found then
    begin
      while (P < Q) and (P^ in [':', ' ']) do
        Inc(P);
      R := P;
      while (P < Q) and (P^ <> #13) do
        Inc(P);
      if P < Q then
        SetString(S, R, P - R)
    end;
    AddItem(LExtractFileName, S);
  finally
    Stream.Free;
  end;
end;

end.
