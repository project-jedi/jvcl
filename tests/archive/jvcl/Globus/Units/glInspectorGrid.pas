unit glInspectorGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, glSGrid, glTypes, ExtCtrls, glCommCl, StdCtrls;

type


  TglGridItem = class(TCollectionItem)
  private
    FCaption: string;
    FEditMask: string;
    FOriginalValues: TStringList;
    FValues: TStringList;
    FExpanded: boolean;
    FSelected: boolean;
    procedure SetCaption(Value: string);
    procedure SetExpanded(Value: boolean);
    procedure SetSelected(Value: boolean);
    procedure SetChanged(Value: boolean);
    function GetChanged: boolean;
    procedure OnValuesChange(Sender: TObject);

    procedure Undo(Index: integer);
    procedure UndoAll;
  public
    Sequence: integer;
    Row: integer;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function GetValue: string;
    procedure SetValue(ValueIndex: integer; const Value: string);
    function IsChanged(ValueIndex: integer): boolean;

    property OriginalValues: TStringList read FOriginalValues write FOriginalValues;
    property EditMask: string read FEditMask write FEditMask;
  published
    property Caption: string read FCaption write SetCaption;
    property Values: TStringList read FValues write FValues;
    property Expanded: boolean read FExpanded write SetExpanded;
    property Selected: boolean read FSelected write SetSelected;
    property Changed: boolean read GetChanged write SetChanged;
  end;

  TglGridItems = class(TCollection)
  private
    FOnUpdate: TNotifyEvent;
    FShowMultiValues: boolean;
    function GetItem(Index: Integer): TglGridItem;
    procedure SetItem(Index: Integer; Value: TglGridItem);
    procedure SetShowMultiValues(Value: boolean);
  protected
     procedure Update(Item: TCollectionItem); override;
     property ShowMultiValues: boolean read FShowMultiValues write SetShowMultiValues;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TglGridItem;
    function Insert(Index: Integer): TglGridItem;

    property Items[Index: Integer]: TglGridItem read GetItem  write SetItem; default;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  published
  end;

  TglInspectorGrid = class(TglStringGrid)
  private
    FItems: TglGridItems;
    procedure ItemsUpdate(Sender: TObject);
  protected
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function CanEditModify: Boolean; override;
//    function GetEditText(ACol, ARow: Longint): String; override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function GetEditMask(ACol, ARow: Longint): string; override;

    procedure DrawButton(ARow: longint; fExpanded: boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;

    procedure GetCellStyle(Sender: TObject; var ACol, ARow: Integer; var Style: TglGridCellStyle);  override;
    procedure GetCellGradientParams(Sender: TObject; ACol, ARow: longint; var CellRect: TRect; var Gradient: TGradient); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UndoCurent;
    procedure UndoAll;

    property Items: TglGridItems read FItems write FItems;
    function RowToItem(ARow: integer): TglGridItem;
    function ItemToRow(Item: TglGridItem): integer;
  published
    { Published declarations }
  end;

procedure Register;

implementation
uses glUtils;
procedure Register;
begin
  RegisterComponents('Gl Controls', [TglInspectorGrid]);
end;

{ TglGridItems }

function TglGridItems.Add: TglGridItem;
begin
  Result := TglGridItem(inherited Add);
end;

constructor TglGridItems.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FShowMultiValues := true;
end;

destructor TglGridItems.Destroy;
begin
  inherited;
end;

function TglGridItems.GetItem(Index: Integer): TglGridItem;
begin
  Result := TglGridItem(inherited Items[Index]);
end;

function TglGridItems.Insert(Index: Integer): TglGridItem;
begin
  Result := TglGridItem(inherited Insert(Index));
end;


procedure TglGridItems.SetItem(Index: Integer; Value: TglGridItem);
begin
  Items[Index].Assign(Value);
end;

procedure TglGridItems.SetShowMultiValues(Value: boolean);
begin
  if FShowMultiValues = Value then exit;
  FShowMultiValues := Value;
  if Assigned(FOnUpdate) then FOnUpdate(self);
end;

procedure TglGridItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnUpdate) then FOnUpdate(self);
end;

{ TglGridItem }

constructor TglGridItem.Create(Collection: TCollection);
begin
  FValues := TStringList.Create;
  FOriginalValues := TStringList.Create;
  FValues.OnChange := OnValuesChange;
  inherited;
end;

destructor TglGridItem.Destroy;
begin
  FValues.Free;
  FOriginalValues.Free;
  inherited;
end;

//==================================================================================================

function TglGridItem.GetChanged: boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to FValues.Count-1 do Result := Result or bool(FValues.Objects[i]);
end;

function TglGridItem.GetValue: string;
var i: integer;
begin
  Result := '';
  if Values.Count = 0 then Result := ''
  else
  if (Values.Count = 1)or(not (Collection as TglGridItems).ShowMultiValues) then Result := Values[0]
  else
  for i := 0 to Values.Count-1 do
  begin
     Result := Result + IIF(i=0, '[', '') + IIF(i>0, ',', '') + Values[i] + IIF(i=Values.Count-1, ']', '');
  end;

end;

function TglGridItem.IsChanged(ValueIndex: integer): boolean;
begin
  if ValueIndex > 0 then dec(ValueIndex);
  Result := bool(Values.Objects[ValueIndex]);
end;

procedure TglGridItem.OnValuesChange(Sender: TObject);
var i: integer;
begin
  while FOriginalValues.Count > FValues.Count do
   FOriginalValues.Delete(FOriginalValues.Count-1);

  for i:=0 to FValues.Count-1 do
  begin
    if i = FOriginalValues.Count then FOriginalValues.Add(FValues[i])
    else if FOriginalValues.Objects[i] = nil then FOriginalValues.Objects[i] := Pointer(1);
  end;

  (Collection as TglGridItems).Update(self);
end;

procedure TglGridItem.SetCaption(Value: string);
begin
  if FCaption = Value then exit;
  FCaption := Value;
  (Collection as TglGridItems).Update(self);
end;

procedure TglGridItem.SetChanged(Value: boolean);
begin
  Values.OnChange := nil;
  if Sequence > 0 then Values.Objects[Sequence-1] := Pointer(1)
                  else Values.Objects[Sequence] := Pointer(1);
  Values.OnChange := OnValuesChange;
end;

procedure TglGridItem.SetExpanded(Value: boolean);
begin
  if FExpanded = Value then exit;
  FExpanded := Value and (Values.Count > 1);
  (Collection as TglGridItems).Update(self);
end;

procedure TglGridItem.SetSelected(Value: boolean);
begin
  if FSelected = Value then exit;
  FSelected := Value;
//  (Collection as TglGridItems).Update(self);
end;

procedure TglGridItem.SetValue(ValueIndex: integer; const Value: string);
var
  i, Seq: integer;
begin
  if (Sequence = 0)and(FValues.Count > 1) then exit;

  if Sequence > 0 then Seq := Sequence-1 else Seq := Sequence;
  if Values[Seq] = Value then exit;

  FValues.OnChange := nil;
  FValues[Seq] := Value;
  FValues.Objects[Seq] := Pointer(integer(Value <> FOriginalValues[Seq]));
  FValues.OnChange := OnValuesChange;

//  FChanged := false;
end;

procedure TglGridItem.Undo(Index: integer);
begin
  FValues[Index] := FOriginalValues[Index];
  FValues.Objects[Index] := Pointer(integer(FValues[Index] <> FOriginalValues[Index]));
end;

procedure TglGridItem.UndoAll;
var i: integer;
begin
  FValues.OnChange := nil;
  for i := 0 to FValues.Count-1 do Undo(i);
  FValues.OnChange := OnValuesChange;
  OnValuesChange(self);
end;

{ TglInspectorGrid }

constructor TglInspectorGrid.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TglGridItems.Create(TglGridItem);
  DefaultDrawing := false;
  DefaultRowHeight := 16;//Canvas.TextHeight('Th');
  ColCount := 2;
  FItems.OnUpdate := ItemsUpdate;
  Options := Options + [goEditing, goAlwaysShowEditor];
  CaptionTextAlignment := taLeftJustify;
end;

destructor TglInspectorGrid.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TglInspectorGrid.DrawButton(ARow: Integer; fExpanded: boolean);
var R: TRect;
begin
  with Canvas do
  begin
    R := Bounds(2, (DefaultRowHeight+1)*ARow + (DefaultRowHeight shr 1)-3, 7, 7);
    FillRect(R);
    Pen.Color := clBlack;
    MoveTo(R.Left+1, R.Top+3);
    LineTo(R.Right-1, R.Top+3);
    if not fExpanded then
    begin
      MoveTo(R.Left+3, R.Top+1);
      LineTo(R.Left+3, R.Bottom-1);
    end;
  end;
end;

procedure TglInspectorGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var Item: TglGridItem;
begin
  inherited;
//  if Items.Count <= ARow then exit;
  if ACol=1 then InvalidateCol(0);

  Item := RowToItem(ARow);
  if Item = nil then exit;

  if (Item.Values.Count > 1)and(Item.Sequence=0) then
    DrawButton(ARow, Item.Expanded);

end;

procedure TglInspectorGrid.GetCellGradientParams(Sender: TObject; ACol,
  ARow: Integer; var CellRect: TRect; var Gradient: TGradient);
begin
  inherited;

end;

procedure TglInspectorGrid.GetCellStyle(Sender: TObject; var ACol, ARow: Integer; var Style: TglGridCellStyle);
var
  i, ItemNo: integer;
  Item:TglGridItem;
begin
  with Style do
  begin
//  inherited;
  ItemNo := 0;
  BevelInner := bvNone;
  BevelOuter := bvSpace;
  BevelBold := false;

  Item := RowToItem(Row);
  if Item = nil then exit;
  
  if ACol=0 then BackgrColor := IIF(ARow = Row, clBtnShadow, DecColor(ColorToRGB(clBtnShadow), 20))
  else BackgrColor := clBtnFace;

  if not Hottracking then
  begin
    FontColor := IIF(ACol=0, clWhite, clBlack);
    if (ACol = 0)and(ARow = Item.Row) then FontColor := clYellow;
  end;
  Interspace := IIF(ACol=0, 13, 3);

  Item := RowToItem(ARow);
  if Item = nil then exit;
  
  if Item.Changed and (Item.IsChanged(Item.Sequence) or (Item.Sequence=0) )then FontStyle := [fsBold] else FontStyle := [];
  end;

end;

{function TglInspectorGrid.GetEditText(ACol, ARow: Longint): string;
var i: integer;
begin
  inherited GetEditText(ACol, ARow);
  for i := 0 to Items.Count-1 do
    Items[i].Selected := false;
  Items[ARow].Selected := true;
end;}

procedure TglInspectorGrid.ItemsUpdate(Sender: TObject);
var i, j: integer;
begin
  SendMessage(handle, WM_SETREDRAW, 0, 0);

  RowCount := 1;
  for i := 0 to Items.Count-1 do
  begin
    Items[i].Row := RowCount-1;
    Cells[0,RowCount-1] := Items[i].Caption;
    if Items[i].Values.Count = 1 then
    begin
      Cells[1,RowCount-1] := Items[i].Values[0];
      if ColCount > 2 then
        Cells[2,RowCount-1] := Items[i].OriginalValues[0];
    end
    else
    if Items[i].Values.Count > 1 then
    begin
      if ColCount > 2 then
        Cells[2,RowCount-1] := '';
      Cells[1,RowCount-1] := Items[i].GetValue;
      if Items[i].Expanded then
        for j:=0 to Items[i].Values.Count - 1 do
        begin

          RowCount := RowCount + 1;
          Cells[0,RowCount-1] := '';
          Cells[1,RowCount-1] := Items[i].Values[j];
          if ColCount > 2 then
            Cells[2,RowCount-1] := Items[i].OriginalValues[j];
        end;
    end
    else Cells[1,RowCount-1] := '';
    RowCount := RowCount + 1;
  end;
  RowCount := RowCount - 1;

  SendMessage(handle, WM_SETREDRAW, 1, 0);
  Invalidate;//Rect(Handle, nil, false);  
end;

procedure TglInspectorGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
type
  PClass = ^TClass;
var
  ClassOld: TClass;
  GridCoord: TGridCoord;
  Item: TglGridItem;
begin
  //...inherit grandfather
{  ClassOld := PClass(self)^;
  PClass(self)^ := self.ClassParent.ClassParent;
  self.MouseDown(Button, Shift, X, Y);
  PClass(self)^ := ClassOld;}
  inherited;

  GridCoord := MouseCoord( X, Y );
  if GridCoord.X = 0 then
  begin
    Item := RowToItem(GridCoord.Y);
    if Item = nil then exit;    
    if Item.Sequence = 0 then
      Item.Expanded := not Item.Expanded;
    Row := GridCoord.Y;
  end;    
end;

function TglInspectorGrid.RowToItem(ARow: integer): TglGridItem;
var i, Index: integer;
begin
  Index := 0; i:=0; Result := nil;
  if Items.Count=0 then exit;
  for i:=0 to ARow-1 do
  begin
    if Items[i].Expanded then inc(Index, Items[i].Values.Count);
    inc(Index);
    if Index > ARow then break;
  end;

  Result := Items[i];
  Result.Sequence := ARow-ItemToRow(Result);

end;

function TglInspectorGrid.ItemToRow(Item: TglGridItem): integer;
var i: integer;
begin
  Result := 0;
  for i:=0 to Item.Index-1 do
  begin
    if Items[i].Expanded then inc(Result, Items[i].Values.Count);
    inc(Result);
  end;
end;

function TglInspectorGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
  if Assigned(OnSelectCell) then OnSelectCell(Self, ACol, ARow, Result);
 // if Items.FShowMultiValues and (Items[ARow].Values.Count > 1) then
//   Result := false;
end;

function TglInspectorGrid.CanEditModify: Boolean;
var Item: TglGridItem;
begin
  if (Col <> 1) or not(goEditing in Options) then
  begin
   Result := false;
   exit;
  end;
  Item := RowToItem(Row);
  if Item = nil then exit;  
  Result := not(Items.FShowMultiValues and (Item.Values.Count > 1) and (Item.Row = Row));
end;

procedure TglInspectorGrid.SetEditText(ACol, ARow: Integer; const Value: string);
var Item: TglGridItem;
begin
  inherited SetEditText(ACol, ARow, Value);
  if Assigned(OnSetEditText) then OnSetEditText(Self, ACol, ARow, Value);
  Item := RowToItem(ARow);
  if Item = nil then exit;
//  if Value <> Item.Values[Item.Sequence] then Item.Changed := true;// Values.Data :=
  Item.SetValue(Item.Sequence, Value);

  if Item.Sequence > 0 then
    Cells[1, Item.Row] := Item.GetValue;
//  Item.Changed := Item.Changed or ;
  //Cells[1, ARow] := Value;
end;

procedure TglInspectorGrid.KeyPress(var Key: Char);
var
  Item: TglGridItem;
  i, OldRow: integer;
begin
  inherited;
  Item := RowToItem(Row);
  if Item = nil then exit;  
  OldRow := Row;
  case Key of
   '+': if (Item.Values.Count > 0)and not Item.Expanded then Item.Expanded := true;
   '-': if (Item.Values.Count > 0)and Item.Expanded and (Row = ItemToRow(Item)) then Item.Expanded := false;
   '*':for i := 0 to Items.Count-1 do Items[i].Expanded := true;
  end;
  Row := OldRow;
end;


procedure TglInspectorGrid.UndoAll;
var i: integer;
begin
  for i := 0 to Items.Count-1 do Items[i].UndoAll;
end;

procedure TglInspectorGrid.UndoCurent;
begin
  RowToItem(Row).UndoAll;
end;

procedure TglInspectorGrid.WMSize(var Msg: TWMSize);
var i, FreeClientWidth: integer;
begin
  inherited;
  FreeClientWidth := Width;
  i := GetScrollPos(handle, SB_VERT);
  if i<>0 then dec(FreeClientWidth, GetSystemMetrics(SM_CXHSCROLL)+2);
  ColWidths[1] := FreeClientWidth - ColWidths[0];
end;

function TglInspectorGrid.GetEditMask(ACol, ARow: Integer): string;
var
  Item: TglGridItem;
begin
  Item := RowToItem(ARow);
  if Item = nil then exit;  
  if CanEditModify then Result := Item.EditMask
                   else Result := '';
  if Assigned(OnGetEditMask) then OnGetEditMask(Self, ACol, ARow, Result);
end;

end.
