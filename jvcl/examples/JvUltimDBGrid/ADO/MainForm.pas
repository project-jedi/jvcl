unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, StdCtrls, ExtCtrls, DBCtrls, JvExStdCtrls,
  JvDBCombobox, Grids, DBGrids, JvExDBGrids, JvDBGrid, JvDBUltimGrid, 
  JvDBGridFooter, ComCtrls;

type
  TfrmMain = class(TForm)
    JvDBGrid1: TJvDBUltimGrid;
    DBMemo1: TDBMemo;
    JvDBComboBox1: TJvDBComboBox;
    DBLookupComboBox1: TDBLookupComboBox;
    Panel1: TPanel;
    B_Connect: TButton;
    B_TitleIndic: TButton;
    B_WordWrap: TButton;
    B_ModFooter: TButton;
    B_ShowEdit: TButton;
    B_Search: TButton;
    ADOConnection1: TADOConnection;
    MainTable: TADOTable;
    MainTableRefLogiciel: TAutoIncField;
    MainTableSoftware: TWideStringField;
    MainTableCategory: TWideStringField;
    MainTableFirstBool: TBooleanField;
    MainTableSecondBool: TBooleanField;
    MainTableLicenses: TSmallintField;
    MainTablePrice: TBCDField;
    MainTableComment: TMemoField;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    LookupTable: TADOTable;
    JvDBGridFooter1: TJvDBGridFooter;
    CountQuery: TADOQuery;
    procedure FormShow(Sender: TObject);
    procedure MainTableCategoryGetText(Sender: TField; var Text: String;
      DisplayText: Boolean);
    procedure B_ModFooterClick(Sender: TObject);
    procedure B_ConnectClick(Sender: TObject);
    procedure B_TitleIndicClick(Sender: TObject);
    procedure B_WordWrapClick(Sender: TObject);
    procedure B_ShowEditClick(Sender: TObject);
    procedure DBMemo1Enter(Sender: TObject);
    procedure JvDBComboBox1KeyPress(Sender: TObject; var Key: Char);
    procedure DBLookupComboBox1KeyPress(Sender: TObject; var Key: Char);
    procedure JvDBGrid1RestoreGridPosition(Sender: TJvDBUltimGrid;
      SavedBookmark: Pointer; SavedRowPos: Integer);
    procedure B_SearchClick(Sender: TObject);
    procedure JvDBGridFooter1Calculate(Sender: TJvDBGridFooter;
      const FieldName: String; var CalcValue: Variant);
  private
    { Private declarations }
    OldRowsHeight, Compteur : Integer;
    DisplayList : TStringList;
    procedure FillUpList;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  
  OldRowsHeight := JvDBGrid1.RowsHeight;
  Compteur := 0;
  DisplayList := TStringList.Create;
end;

destructor TfrmMain.Destroy;
begin
  DisplayList.Free;
  inherited;
end;

procedure TfrmMain.FillUpList;
begin
  if ((DisplayList = nil) or (not LookupTable.Active)) then
    Exit;
  DisplayList.Clear;
  LookupTable.First;
  while (not LookupTable.Eof) do
  begin
    DisplayList.Add(LookupTable.FieldByName('CodeLogiciel').AsString +
             '=' + LookupTable.FieldByName('LibelleLog').AsString);
    LookupTable.Next;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  InitialSort: TSortFields;
begin
  SetLength(InitialSort, 3);
  InitialSort[0].Name := 'Category';
  InitialSort[0].Order := JvGridSort_ASC;
  InitialSort[1].Name := 'Licenses';
  InitialSort[1].Order := JvGridSort_DESC;
  InitialSort[2].Name := 'Software';
  InitialSort[2].Order := JvGridSort_ASC;
  JvDBGrid1.Sort(InitialSort);
end;

procedure TfrmMain.MainTableCategoryGetText(Sender: TField;
  var Text: String; DisplayText: Boolean);
begin
  if not Assigned(DisplayList) then
    Exit;
  if (DisplayList.IndexOfName(Sender.AsString) = -1) then
    FillUpList; // Key value not found -> the list is (re)loaded
  Text := DisplayList.Values[Sender.AsString];
end;

procedure TfrmMain.B_ModFooterClick(Sender: TObject);
begin
  JvDBGridFooter1.Columns.Items[0].Alignment := taCenter;
  JvDBGridFooter1.Columns.Items[0].Bevel := pbRaised;
  JvDBGridFooter1.Columns.Items[1].FieldName := 'Category';
  JvDBGridFooter1.Columns.Items[1].DisplayMask := '';
  JvDBGridFooter1.IgnoreHorzScrolling := True;
  JvDBGridFooter1.IgnoreResizing := True;
  JvDBGrid1.FixedCols := 1;
  B_ModFooter.Enabled := False;
end;

procedure TfrmMain.B_ConnectClick(Sender: TObject);
begin
  ADOConnection1.Connected := not ADOConnection1.Connected;
  MainTable.Active := ADOConnection1.Connected;
  LookupTable.Active := ADOConnection1.Connected;
end;

procedure TfrmMain.B_TitleIndicClick(Sender: TObject);
begin
  Inc(Compteur);
  if (Compteur = 1) then
  begin
    JvDBGrid1.Options := JvDBGrid1.Options - [dgTitles];
  end
  else if (Compteur = 2) then
  begin
    JvDBGrid1.Options := JvDBGrid1.Options - [dgIndicator];
  end
  else if (Compteur = 3) then
  begin
    JvDBGrid1.Options := JvDBGrid1.Options + [dgTitles];
  end
  else if (Compteur = 4) then
  begin
    JvDBGrid1.Options := JvDBGrid1.Options + [dgIndicator];
    Compteur := 0;
  end;
end;

procedure TfrmMain.B_WordWrapClick(Sender: TObject);
begin
  JvDBGrid1.WordWrap := not JvDBGrid1.WordWrap;
end;

procedure TfrmMain.B_ShowEditClick(Sender: TObject);
begin
  if (dgAlwaysShowEditor in JvDBGrid1.Options) then
    JvDBGrid1.Options := JvDBGrid1.Options - [dgAlwaysShowEditor]
  else
    JvDBGrid1.Options := JvDBGrid1.Options + [dgAlwaysShowEditor];
end;

procedure TfrmMain.DBMemo1Enter(Sender: TObject);
begin
  // Text is selected automatically
  TDBMemo(Sender).SelectAll;
end;

procedure TfrmMain.JvDBComboBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    JvDBGrid1.CloseControl;
end;

procedure TfrmMain.DBLookupComboBox1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key = #13) then
    JvDBGrid1.CloseControl
  else
  if (Key = #27) then
    DBLookupComboBox1.Field.Value := DBLookupComboBox1.Field.OldValue;
end;

procedure TfrmMain.JvDBGrid1RestoreGridPosition(Sender: TJvDBUltimGrid;
  SavedBookmark: Pointer; SavedRowPos: Integer);
begin
  // Perfect cursor replacement
  if (MainTable.BookmarkValid(SavedBookmark)) then
    MainTable.Recordset.Bookmark := POleVariant(SavedBookmark)^;
  try
    MainTable.Resync([rmExact]);
  except
  end;
end;

procedure TfrmMain.B_SearchClick(Sender: TObject);
var
  ResultCol: Integer;
  ResultField: TField;
  Found: Boolean;
begin
  // Search of W (uppercase) in Software field, then search of 1 in Licenses field
  Found := false;
  JvDBGrid1.SaveGridPosition;
  JvDBGrid1.SearchFields.Clear;
  JvDBGrid1.SearchFields.Add('Software');
  if (JvDBGrid1.Search('W', ResultCol, ResultField, True, False, True)) then
  begin
    Found := (MainTable.FieldByName('Licenses').AsString = '1');
    while (not Found) do
    begin
      if (not JvDBGrid1.SearchNext(ResultCol, ResultField, True, False, True)) then
        break;
      Found := (MainTable.FieldByName('Licenses').AsString = '1');
    end;
  end;
  if (Found) then
    ShowMessage('Result found:'#13#10 + ResultField.AsString)
  else
  begin
    JvDBGrid1.RestoreGridPosition();
    ShowMessage('Not found');
  end;
end;

procedure TfrmMain.JvDBGridFooter1Calculate(Sender: TJvDBGridFooter;
  const FieldName: String; var CalcValue: Variant);
var
  C: Integer;
begin
  if (MainTable.Active) then
  begin
    if (AnsiSameText(FieldName, 'Licenses')) then
    begin
      CountQuery.Open();
      if (CountQuery.Eof) then
        CalcValue := 'ERROR'
      else
        CalcValue := CountQuery.FieldByName('Total').AsInteger;
      CountQuery.Close();
    end
    else
    if (AnsiSameText(FieldName, 'Category')) then
    begin
      CalcValue := string('');
      for C := 0 to JvDBGrid1.Columns.Count-1 do
      begin
        if (JvDBGrid1.Columns.Items[C].Visible) then
        begin
          if (CalcValue <> string('')) then
            CalcValue := CalcValue + string(',');
          CalcValue := CalcValue +
            IntToStr(JvDBGrid1.Columns.Items[C].Width);
        end;
      end;
      CalcValue := 'Widths = ' + CalcValue;
    end
    else
      CalcValue := MainTable.RecordCount;
  end
  else
    CalcValue := FieldName;
end;

end.
