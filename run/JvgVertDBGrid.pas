{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgVertDBGrid.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgVertDBGrid;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, Grids, DB, SysUtils,
  JvgTypes, JvgCommClasses, JvgUtils, JVCLVer, JvgStringGrid;

type
  TJvgCustomVertDBSGrid = class(TJvgStringGrid)
  private
    FDataSource: TDataSource;
    FDataSet: TDataSet;
    FShowFromFieldNo: Word;
    FNumFieldsToShow: Word;
    FIgnoreSetText: Boolean;
    FLastEditTextRow: Integer;
    FLastEditValue: string;
    procedure SetDataToField(Field: TField; Str: string);
    function GetDataFromField(Field: TField): string;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDataSet(Value: TDataSet);
    procedure SetShowFromFieldNo(Value: Word);
    procedure SetNumFieldsToShow(Value: Word);
    procedure SaveEditTextToDB;
    procedure UndoEditText;
    procedure DataChange(Sender: TObject; Field: TField);
  protected
    property DataSet: TDataSet read FDataSet write SetDataSet;
    procedure DoExit; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property ShowFromFieldNo: Word read FShowFromFieldNo write SetShowFromFieldNo default 0;
    property NumFieldsToShow: Word read FNumFieldsToShow write SetNumFieldsToShow default 100;
  public
    constructor Create(AOwner: TComponent); override;
    procedure FillGridWithData;
  end;

  TJvgVertDBSGrid = class(TJvgCustomVertDBSGrid)
  private
    FAboutJVCL: TJVCLAboutInfo;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property DataSource;
    property ShowFromFieldNo;
    property NumFieldsToShow;
  end;

implementation

constructor TJvgCustomVertDBSGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNumFieldsToShow := 100;
  ColCount := 2;
  FLastEditTextRow := -1;
end;

procedure TJvgCustomVertDBSGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  if (FLastEditTextRow = ARow) and (FDataSet.State <> dsEdit) and
    (FDataSet.Fields[ARow + ShowFromFieldNo].AsString <> FLastEditValue) then
    FDataSet.Edit;

  if FIgnoreSetText then
  begin
    FIgnoreSetText := False;
    Exit;
  end;
  FLastEditValue := Value;
  FLastEditTextRow := ARow;
  if FDataSet.State = dsEdit then
    SaveEditTextToDB;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TJvgCustomVertDBSGrid.SaveEditTextToDB;
begin
  if FLastEditTextRow >= 0 then
    with FDataSet do
    begin
      if Fields[FLastEditTextRow + ShowFromFieldNo].Value = FLastEditValue then
        Exit;
      if State <> dsEdit then
        Edit;
      SetDataToField(Fields[FLastEditTextRow + ShowFromFieldNo], FLastEditValue);
      FDataSet.Post;
      //FIgnoreSetText := True;
      //Cells[1,FLastEditTextRow] := FLastEditValue;
    end;
end;

procedure TJvgCustomVertDBSGrid.UndoEditText;
begin
  if FLastEditTextRow >= 0 then
  begin
    with FDataSet do
      Cells[1, FLastEditTextRow] := GetDataFromField(Fields[FLastEditTextRow + ShowFromFieldNo]);
    FLastEditTextRow := -1;
  end;
end;

procedure TJvgCustomVertDBSGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      if EditorMode then
      begin
        FIgnoreSetText := True;
        //EditorMode := False;
        UndoEditText;
      end;
    VK_LEFT:
      InplaceEditor.SelLength := 0;
    VK_RIGHT:
      begin
        InplaceEditor.SelStart := InplaceEditor.SelLength;
        InplaceEditor.SelLength := 0;
      end;
    {VK_UP, VK_DOWN,} VK_RETURN:
      begin
        SaveEditTextToDB;
        inherited KeyDown(Key, Shift);
      end;
  else
    inherited KeyDown(Key, Shift);
  end;
end;

{procedure TJvgCustomVertDBSGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
const
  aAlignments: array[TAlignment] of Longint = ( ES_LEFT, ES_RIGHT, ES_CENTER );
var
  R: TRect;
  S: string;
begin
  if (ACol = 0)and(FixedCols>0) then
  begin
//    DefaultDrawing := False;
//    DefaultDrawing := True;
    InflateRect(ARect, -1, -1 );
    Canvas.Font := FCaptFont;
    R := ARect;
    if Cells[ACol,ARow]<>'' then S := Cells[ACol,ARow]
       else try S := Captions[ARow]; except S:=''; end;
    DrawText( Canvas.Handle, PChar(S), length(S), R, aAlignments[FAlignment] or DT_WORDBREAK or
       DT_CALCRECT );
    ARect.Top := max( ARect.Top, ARect.Top+(ARect.Bottom-ARect.Top-(R.Bottom-R.Top)) div 2 );
    DrawText( Canvas.Handle, PChar(S), length(S), ARect, aAlignments[FAlignment] or DT_WORDBREAK);
    //  DT_CENTER or DT_WORDBREAK);
 end
 else inherited;
end;
}

procedure TJvgCustomVertDBSGrid.SetDataToField(Field: TField; Str: string);
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

function TJvgCustomVertDBSGrid.GetDataFromField(Field: TField): string;
begin
  case Field.DataType of
    ftString:
      Result := Field.AsString;
    ftFloat, ftCurrency:
      Result := FloatToStr(Field.AsFloat);
  else
    Result := IntToStr(Field.AsInteger);
  end;
end;

procedure TJvgCustomVertDBSGrid.SetDataSource(Value: TDataSource);
begin
  FDataSource := Value;
  if not Assigned(FDataSource) then
    DataSet := nil
  else
  begin
    FDataSource.OnDataChange := DataChange;
    DataSet := FDataSource.DataSet;
  end;
end;

procedure TJvgCustomVertDBSGrid.SetDataSet(Value: TDataSet);
begin
  FDataSet := Value;
  if not Assigned(DataSet) then
  begin
    //    Cells[0,0] := '';
    //    for i:=0 to RowCount-1 do Cells[1,i] := '';
    //    Captions.Clear;
    RowCount := 0; // ColCount := 0;
  end
  else
    FillGridWithData;
end;

procedure TJvgCustomVertDBSGrid.SetShowFromFieldNo(Value: Word);
begin
  FShowFromFieldNo := Value;
  FillGridWithData;
end;

procedure TJvgCustomVertDBSGrid.SetNumFieldsToShow(Value: Word);
begin
  FNumFieldsToShow := Value;
  FillGridWithData;
end;

procedure TJvgCustomVertDBSGrid.DoExit;
begin
  if Assigned(FDataSet) then
    if FDataSet.State = dsEdit then
      FDataSet.Post;
  inherited DoExit;
end;

procedure TJvgCustomVertDBSGrid.FillGridWithData;
var
  I, J: Integer;
begin
  if Assigned(FDataSet) then
    with FDataSet do
    begin
      RowCount := FieldCount; // RowCount := min( FieldCount - ShowFromFieldNo, NumFieldsToShow );
      J := 0;
      for I := 0 {ShowFromFieldNo} to RowCount - 1 {+ShowFromFieldNo} do //max( FieldCount-1, RowCount-1 ) do
      begin
        //if I >= RowCount then break;
        //if I >= FieldCount then Cells[1,I] := ''
        //else
        //if Cells[0,j]='' then Cells[0,j] := Fields[i].Text;
        Cells[1, J] := Fields[I].AsString; //GetDataFromField(Fields[i]);
        Inc(J);
      end;
      //Repaint;
    end;
end;

procedure TJvgCustomVertDBSGrid.DataChange(Sender: TObject; Field: TField);
begin
  if DataSet <> FDataSource.DataSet then
    DataSet := FDataSource.DataSet;
  //  if field = nil then FillGridWithData;
end;

end.

