unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Data.DB, Datasnap.DBClient,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids,
  JvExDBGrids, JvDBGrid;

type
  TForm1 = class(TForm)
    cdsItems: TClientDataSet;
    cdsItemsID: TIntegerField;
    cdsItemsLISTID: TIntegerField;
    cdsItemsLISTTYPE: TIntegerField;
    cdsItemsOMSCHR: TStringField;
    cdsList1: TClientDataSet;
    cdsList1EX: TStringField;
    cdsList1ID: TIntegerField;
    cdsList1OMSCHR: TStringField;
    cdsList2: TClientDataSet;
    cdsListTypes: TClientDataSet;
    dsItems: TDataSource;
    IntegerField1: TIntegerField;
    IntegerField2: TIntegerField;
    JvDBGrid1: TJvDBGrid;
    StringField1: TStringField;
    StringField2: TStringField;
    procedure cdsItemsAfterOpen(DataSet: TDataSet);
    procedure cdsItemsLISTIDGetText(Sender: TField; var Text: string; DisplayText:
        Boolean);
    procedure cdsItemsLISTTYPEGetText(Sender: TField; var Text: string;
        DisplayText: Boolean);
    procedure cdsList1AfterOpen(DataSet: TDataSet);
    procedure cdsList2AfterOpen(DataSet: TDataSet);
    procedure cdsListTypesAfterOpen(DataSet: TDataSet);
    procedure dsItemsDataChange(Sender: TObject; Field: TField);
    function GetLookupDsByType(aType: Integer): TDataset;
    procedure JvDBGrid1GetColumnLookupInfo(Sender: TObject; Column: TColumn; var
        LookupInfo: TJvDBGridColumnLookupInfo);
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.DBCtrls;

{$R *.dfm}

{ TForm1 }

procedure TForm1.cdsItemsAfterOpen(DataSet: TDataSet);
begin
  // Some demo records
  DataSet.AppendRecord([1, 'Test 1', 1, 3]);
  DataSet.AppendRecord([2, 'Test 2', 1, 2]);
  DataSet.AppendRecord([3, 'Test 3', 2, 13]);
  DataSet.AppendRecord([4, 'Test 4', 2, 12]);
  DataSet.AppendRecord([5, 'Test 5', 2, 14]);
end;

procedure TForm1.cdsItemsLISTIDGetText(Sender: TField; var Text: string;
    DisplayText: Boolean);
var
  ds: TDataset;
begin
  if not Sender.IsNull then
  begin
    ds := GetLookupDsByType(cdsItemsLISTTYPE.AsInteger);
    if Assigned(ds) then
      Text := VarToStr(ds.Lookup(Sender.LookupKeyFields, Sender.Value, Sender.LookupResultField));
  end;
end;

procedure TForm1.cdsItemsLISTTYPEGetText(Sender: TField; var Text: string;
    DisplayText: Boolean);
begin
  if not Sender.IsNull then
    Text := VarToStr(Sender.LookupDataSet.Lookup(Sender.LookupKeyFields, Sender.Value, Sender.LookupResultField));
end;

procedure TForm1.cdsList1AfterOpen(DataSet: TDataSet);
begin
  DataSet.AppendRecord([1, 'L1-1']);
  DataSet.AppendRecord([2, 'L1-2']);
  DataSet.AppendRecord([3, 'L1-3']);
  DataSet.AppendRecord([4, 'L1-4']);
end;

procedure TForm1.cdsList2AfterOpen(DataSet: TDataSet);
begin
  DataSet.AppendRecord([11, 'L2-1']);
  DataSet.AppendRecord([12, 'L2-2']);
  DataSet.AppendRecord([13, 'L2-3']);
  DataSet.AppendRecord([14, 'L2-4']);
end;

procedure TForm1.cdsListTypesAfterOpen(DataSet: TDataSet);
begin
  DataSet.AppendRecord([1, 'List 1']);
  DataSet.AppendRecord([2, 'List 2']);
end;

procedure TForm1.dsItemsDataChange(Sender: TObject; Field: TField);
begin
  if (Field = cdsItemsLISTTYPE) and (not cdsItemsLISTID.IsNull) then
    cdsItemsLISTID.Clear;
end;

function TForm1.GetLookupDsByType(aType: Integer): TDataset;
begin
  case aType of
    1: Result := cdsList1;
    2: Result := cdsList2;
  else
    Result := nil;
  end;
end;

procedure TForm1.JvDBGrid1GetColumnLookupInfo(Sender: TObject; Column: TColumn;
    var LookupInfo: TJvDBGridColumnLookupInfo);
begin
  if Column.Field = cdsItemsLISTID then
  begin
    LookupInfo.IsLookup := True;
    LookupInfo.LookupDataSet := GetLookupDsByType(cdsItemsLISTTYPE.AsInteger);
  end else
    LookupInfo.IsLookup := Column.Field.KeyFields > '';
end;

end.
