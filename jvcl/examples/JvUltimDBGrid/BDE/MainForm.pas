unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, DBCtrls, StdCtrls, Buttons, ExtCtrls, Grids,
  DBGrids, JvExDBGrids, JvDBGrid, JvDBUltimGrid;

type
  TfrmMain = class(TForm)
    MyUltimGrid: TJvDBUltimGrid;
    Panel1: TPanel;
    Label1: TLabel;
    ValueToSearch: TEdit;
    B_Search: TBitBtn;
    B_SearchNext: TBitBtn;
    DBNavigator1: TDBNavigator;
    B_ResizeCols: TBitBtn;
    Table1: TTable;
    DataSource1: TDataSource;
    procedure FormShow(Sender: TObject);
    procedure MyUltimGridIndexNotFound(Sender: TJvDBUltimGrid;
      FieldsToSort: TSortFields; IndexFieldNames, DescFields: String;
      var Retry: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure B_SearchClick(Sender: TObject);
    procedure B_SearchNextClick(Sender: TObject);
    procedure MyUltimGridDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure B_ResizeColsClick(Sender: TObject);
  private
    { Private declarations }
   ResultCol: Integer;
   ResultField: TField;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormShow(Sender: TObject);
var
  InitialSort: TSortFields;
begin
  Table1.Open;
  SetLength(InitialSort, 2);
  InitialSort[0].Name := 'Category';
  InitialSort[0].Order := JvGridSort_ASC;
  InitialSort[1].Name := 'Common_Name';
  InitialSort[1].Order := JvGridSort_ASC;
  MyUltimGrid.Sort(InitialSort);
end;

procedure TfrmMain.MyUltimGridIndexNotFound(Sender: TJvDBUltimGrid;
  FieldsToSort: TSortFields; IndexFieldNames, DescFields: String;
  var Retry: Boolean);
var
  NewIndex: Integer;
  DescMarker: string;
  NewIndexName: string;
begin
  // Nota bene: you need to update Biolife.db to Paradox 7 if you want a sort
  //            in descending order. Otherwise, the index creation will fail.
  DescMarker := '';
  if DescFields = '' then
    DescMarker := ' (DESC)';
  NewIndex := Application.MessageBox(PChar('Index not found for field: ' +
              IndexFieldNames + DescMarker +
              #13#10'Do you want to create an index ?'), 'New index ?', MB_YESNO);
  if (NewIndex = ID_YES) then
  begin
    NewIndexName := IndexFieldNames;
    while (Pos(';', NewIndexName) > 0) do
       Delete(NewIndexName, Pos(';', NewIndexName), 1);
    while (Pos(' ', NewIndexName) > 0) do
       Delete(NewIndexName, Pos(' ', NewIndexName), 1);
    NewIndexName := 'Idx_' + NewIndexName + DescMarker;

    Table1.Close;
    try
       Table1.AddIndex(NewIndexName, IndexFieldNames,
                        [ixCaseInsensitive], DescFields);
    except
      on ErrAddIdx: Exception do
        ShowMessage('AddIndex(' + NewIndexName + ') failed: ' + ErrAddIdx.Message);
    end;
    Table1.Open;
    Retry := true;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Table1.Close;
end;

procedure TfrmMain.B_SearchClick(Sender: TObject);
var
  SearchOk: Boolean;
begin
  MyUltimGrid.SearchFields.Clear();
  MyUltimGrid.SearchFields.Add('Category');
  MyUltimGrid.SearchFields.Add('Common_Name');
  MyUltimGrid.SearchFields.Add('Species Name');
  MyUltimGrid.SearchFields.Add('Notes');
  SearchOK := MyUltimGrid.Search(ValueToSearch.Text, ResultCol, ResultField,
                                     False, False, True);
  B_SearchNext.Enabled := SearchOK;
  if (SearchOK) then
    ShowMessage('Hurrah! Text found in field ' + Trim(ResultField.FieldName))
  else
    ShowMessage('Text not found');
end;

procedure TfrmMain.B_SearchNextClick(Sender: TObject);
begin
  if (not MyUltimGrid.SearchNext(ResultCol, ResultField, False, False, True)) then
  begin
    ShowMessage('Text not found - End of search');
    B_SearchNext.Enabled := False;
  end;
end;

procedure TfrmMain.MyUltimGridDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn;
  State: TGridDrawState);
var
  Image: TBitmap;
begin
  // Hey fish, let me see your face...hum...your fins
  if (Column.Field.FieldName = 'Graphic') then
  begin
    Image := TBitmap.Create;
    try
      Image.Assign(TBlobField(Column.Field));
      if (Image.Height > Rect.Bottom - Rect.Top) then
        Image.Height := Rect.Bottom - Rect.Top;
      if (Image.Width > Rect.Right - Rect.Left) then
        Image.Width := Rect.Right - Rect.Left;
      MyUltimGrid.Canvas.FillRect(Rect);
      MyUltimGrid.Canvas.Draw(Rect.Left, Rect.Top, Image);
    finally
      Image.Free;
    end;
  end;
end;

procedure TfrmMain.B_ResizeColsClick(Sender: TObject);
begin
  MyUltimGrid.RowsHeight := 129;
  MyUltimGrid.InitializeColumnsWidth(70, False, [36, 76]);
  MyUltimGrid.AutoSizeColumnIndex := JvGridResizeLastVisibleCol;
  MyUltimGrid.AutoSizeColumns := True;
end;

end.
