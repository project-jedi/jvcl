unit glVDBGrd;

interface
uses
  Windows, Messages, Classes, Controls, Graphics, glTypes, glCommCl,
  glUtils, grids, glSGrid, DB, SysUtils;

type

  TglCustomVertDBSGrid = class(TglStringGrid)
  private
    FDataSource 	: TDataSource;
    FDataSet		: TDataSet;
    FShowFromFieldNo	: word;
    FNumFieldsToShow	: word;
    fIgnoreSetText	: boolean;
    LastEditTextRow	: integer;
    LastEditValue	: string;
    procedure SetDataToFiled( Field: TField; Str: string );
    function GetDataFromFiled( Field: TField ): string;

    procedure SetDataSource(Value: TDataSource);
    procedure SetDataSet(Value: TDataSet);
    procedure SetShowFromFieldNo(Value: word);
    procedure SetNumFieldsToShow(Value: word);
    procedure SaveEditTextToDB;
    procedure UndoEditText;
    procedure DataChange(Sender: TObject; Field: TField);
  protected

    property DataSet: TDataSet read FDataSet write SetDataSet;
    procedure DoExit; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

//    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure FillGridWithData;
  protected
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property ShowFromFieldNo: word read FShowFromFieldNo write SetShowFromFieldNo
     default 0;
    property NumFieldsToShow: word read FNumFieldsToShow write SetNumFieldsToShow
     default 100;
  end;

  TglVertDBSGrid = class(TglCustomVertDBSGrid)
  published
    property DataSource;
    property ShowFromFieldNo;
    property NumFieldsToShow;
  end;

  procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure Register;
begin
  RegisterComponents('Proba', [TglVertDBSGrid]);
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}

constructor TglCustomVertDBSGrid.Create( AOwner : TComponent );
begin
  inherited;
  FNumFieldsToShow := 100;
  ColCount := 2;
  LastEditTextRow := -1;
end;

destructor TglCustomVertDBSGrid.Destroy;
begin
  inherited;
end;

procedure TglCustomVertDBSGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  if (LastEditTextRow = ARow)and
     (FDataSet.State <> dsEdit)and
     (FDataSet.Fields[ARow+ShowFromFieldNo].AsString <> LastEditValue)
  then FDataSet.Edit;

  if fIgnoreSetText then begin fIgnoreSetText := false; exit; end;
  LastEditValue := Value;
  LastEditTextRow := ARow;
  if FDataSet.State = dsEdit then SaveEditTextToDB;
  inherited;
end;

procedure TglCustomVertDBSGrid.SaveEditTextToDB;
begin
  if LastEditTextRow < 0 then exit;
  with FDataSet do
  begin
    if Fields[LastEditTextRow+ShowFromFieldNo].Value = LastEditValue then exit;
    if State <> dsEdit then Edit;
    SetDataToFiled( Fields[LastEditTextRow+ShowFromFieldNo], LastEditValue );
    FDataSet.Post;
    //fIgnoreSetText := true;
    //Cells[1,LastEditTextRow] := LastEditValue;
  end;
end;

procedure TglCustomVertDBSGrid.UndoEditText;
begin
  if LastEditTextRow < 0 then exit;
  with FDataSet do
    Cells[1,LastEditTextRow] := GetDataFromFiled( Fields[LastEditTextRow+ShowFromFieldNo] );
  LastEditTextRow := -1;
end;

procedure TglCustomVertDBSGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      if EditorMode then
      begin
	fIgnoreSetText := true;
	//EditorMode := false;
	UndoEditText;
      end;
      VK_LEFT: InplaceEditor.SelLength := 0;
      VK_RIGHT: begin InplaceEditor.SelStart := InplaceEditor.SelLength; InplaceEditor.SelLength := 0; end;
      {VK_UP, VK_DOWN,} VK_RETURN: begin SaveEditTextToDB; inherited; end;
      else inherited;
  end;
end;
{procedure TglCustomVertDBSGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
const
  aAlignments: array[TAlignment] of Longint = ( ES_LEFT, ES_RIGHT, ES_CENTER );
var
  R: TRect;
  S: string;
begin
  if (ACol = 0)and(FixedCols>0) then
  begin
//    DefaultDrawing := false;
//    DefaultDrawing := true;
    InflateRect(ARect, -1, -1 );
    Canvas.Font := FCaptFont;
    R := ARect;
    if Cells[ACol,ARow]<>'' then S := Cells[ACol,ARow]
			    else try S := Captions[ARow]; except S:=''; end;
    DrawText( Canvas.Handle, PChar(S), length(S), R, aAlignments[FAlignment] or DT_WORDBREAK or
	      DT_CALCRECT );
    ARect.Top := max( ARect.Top, ARect.Top+(ARect.Bottom-ARect.Top-(R.Bottom-R.Top)) div 2 );
    DrawText( Canvas.Handle, PChar(S), length(S), ARect, aAlignments[FAlignment] or DT_WORDBREAK );
//		DT_CENTER or DT_WORDBREAK );
 end
 else inherited;

end;
}

procedure TglCustomVertDBSGrid.SetDataToFiled( Field: TField; Str: string );
begin
    case Field.DataType of
      ftString:
	Field.AsString := Str;
      ftFloat, ftCurrency:
	try
	  Field.AsFloat := StrToFloat(Str);
	except
	  Field.AsFloat := 0;
	end;
      else
	try
	  Field.AsInteger := StrToInt(Str);
	except
	  Field.AsInteger := 0;
	end;
    end;
end;

function TglCustomVertDBSGrid.GetDataFromFiled( Field: TField ): string;
begin
  case Field.DataType of
    ftString: Result := Field.AsString;
    ftFloat, ftCurrency: Result := FloatToStr( Field.AsFloat );
    else Result := IntToStr( Field.AsInteger );
  end;
end;

procedure TglCustomVertDBSGrid.SetDataSource(Value: TDataSource);
var i: integer;
begin
  FDataSource := Value;
  if not Assigned(FDataSource) then
  begin DataSet := nil; exit; end;
  FDataSource.OnDataChange := DataChange;
  DataSet := FDataSource.DataSet;
end;

procedure TglCustomVertDBSGrid.SetDataSet(Value: TDataSet);
var i: integer;
begin
  FDataSet := Value;
  if not Assigned(DataSet) then
  begin
//    Cells[0,0] := '';
//    for i:=0 to RowCount-1 do Cells[1,i] := '';
//    Captions.Clear;
    RowCount := 0;// ColCount := 0;
    exit;
  end else FillGridWithData;
end;

procedure TglCustomVertDBSGrid.SetShowFromFieldNo(Value: word);
begin
  FShowFromFieldNo := Value;
  FillGridWithData;
end;

procedure TglCustomVertDBSGrid.SetNumFieldsToShow(Value: word);
begin
  FNumFieldsToShow := Value;
  FillGridWithData;
end;

procedure TglCustomVertDBSGrid.DoExit;
begin
  if Assigned(FDataSet) then
  if FDataSet.State = dsEdit then FDataSet.Post;
  inherited;
end;

procedure TglCustomVertDBSGrid.FillGridWithData;
var i, j: integer;
begin
  if Assigned(FDataSet) then with FDataSet do
  begin
    RowCount := FieldCount;//	 RowCount := min( FieldCount - ShowFromFieldNo, NumFieldsToShow );
    j := 0;
    for i :=0{ShowFromFieldNo} to RowCount-1{+ShowFromFieldNo} do//max( FieldCount-1, RowCount-1 ) do
    begin
      //if i >= RowCount then break;
      //if i >= FieldCount then Cells[1,i] := ''
      //else
      //if Cells[0,j]='' then Cells[0,j] := Fields[i].Text;
      Cells[1,j] := Fields[i].AsString;//GetDataFromFiled(Fields[i]);
      inc(j);
    end;
    //Repaint;
  end;
end;

procedure TglCustomVertDBSGrid.DataChange(Sender: TObject; Field: TField);
begin
  if DataSet <> FDataSource.DataSet then DataSet := FDataSource.DataSet;
//  if field = nil then FillGridWithData;
end;

end.

