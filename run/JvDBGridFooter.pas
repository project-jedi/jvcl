{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBGridFooter.PAS, released on 2004-08-13.

The Initial Developers of the Original Code are: Frédéric Leneuf-Magaud
Copyright (c) 2004 Frédéric Leneuf-Magaud
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

-----------------------------------------------------------------------------
HOW TO USE THIS COMPONENT:
-----------------------------------------------------------------------------

- Put a JvDBGrid or JvDBUltimGrid onto your form;
- Link this component to your grid with the DBGrid property (the DataSource
  property is automatically set);
- Open the columns property editor;
- Add any column you need and set the Fieldname property for every column;
- Assign the OnCalculate event to your calculation function.

-----------------------------------------------------------------------------

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBGridFooter;

{$I jvcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Sysutils, Classes, ComCtrls,
  DB, DBGrids, Grids, JvDBGrid;

type
  TJvDBGridFooter = class;

  TFooterDataLink = class(TDataLink)
  private
    FGridFooter: TJvDBGridFooter;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure LayoutChanged; override;
  public
    constructor Create(AFooter: TJvDBGridFooter);
    destructor Destroy; override;
  end;

  TFooterColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FBevel: TStatusPanelBevel;
    FBiDiMode: TBiDiMode;
    FDisplayMask: string;
    FFieldName: string;
    FParentBiDiMode: Boolean;
    FStyle: TStatusPanelStyle;
    FWidthIfIgnore: Integer;
    FCurrentValue: Variant;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: TStatusPanelBevel);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetDisplayMask(Value: string);
    procedure SetFieldName(Value: string);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetStyle(Value: TStatusPanelStyle);
    procedure SetWidthIfIgnore(Value: Integer);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode default bdLeftToRight;
    property DisplayMask: string read FDisplayMask write SetDisplayMask;
    property FieldName: string read FFieldName write SetFieldName;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode
      default True;
    property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
    property WidthIfIgnore: Integer read FWidthIfIgnore write SetWidthIfIgnore default 64;
  end;

  TFooterColumns = class(TCollection)
  private
    FFooterBar: TJvDBGridFooter;
    function GetItem(Index: Integer): TFooterColumn;
    procedure SetItem(Index: Integer; Value: TFooterColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(FooterBar: TJvDBGridFooter);
    function Add: TFooterColumn;
    property Items[Index: Integer]: TFooterColumn read GetItem write SetItem;
  end;

  TCalculateEvent = procedure(Sender: TJvDBGridFooter; const FieldName: string;
    var CalcValue: Variant) of object;
  TDisplayTextEvent = procedure(Sender: TJvDBGridFooter; Column: TFooterColumn;
    const Value: Variant; var Text: string) of object;

  TJvDBGridFooter = class(TStatusBar)
  private
    FColumns: TFooterColumns;
    FDataLink: TFooterDataLink;
    FDBGrid: TJvDBGrid;
    FIgnoreHorzScrolling: Boolean;
    FIgnoreResizing: Boolean;
    FOnCalculate: TCalculateEvent;
    FOnDisplayText: TDisplayTextEvent;
    
    FJvDBGridLayoutChangeLink: TJvDBGridLayoutChangeLink;
    
    procedure SetColumns(Value: TFooterColumns);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDBGrid(Value: TJvDBGrid);
    procedure SetIgnoreHorzScrolling(Value: Boolean);
    procedure SetIgnoreResizing(Value: Boolean);
  protected
    procedure JvDBGridLayoutChanged(Grid: TJvDBGrid; Kind: TJvDBGridLayoutChangeKind); dynamic;
    
    procedure ReCalc;
    procedure DrawPanels; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Columns: TFooterColumns read FColumns write SetColumns;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DBGrid: TJvDBGrid read FDBGrid write SetDBGrid;
    property IgnoreHorzScrolling: Boolean read FIgnoreHorzScrolling
      write SetIgnoreHorzScrolling default False;
    property IgnoreResizing: Boolean read FIgnoreResizing write SetIgnoreResizing
      default False;
    property Panels stored False; // This property of TStatusBar is hidden
    property SizeGrip default False;
    property OnCalculate: TCalculateEvent read FOnCalculate write FOnCalculate;
    property OnDisplayText: TDisplayTextEvent read FOnDisplayText write FOnDisplayText;
  end;     

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{ TFooterColumn }

constructor TFooterColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAlignment := taCenter;
  FBevel := pbLowered;
  FBiDiMode := bdLeftToRight;
  FDisplayMask := '';
  FFieldName := '';
  FParentBiDiMode := True;
  FStyle := psText;
  FWidthIfIgnore := 64;
  FCurrentValue := Null;
end;

procedure TFooterColumn.Assign(Source: TPersistent);
begin
  if Source is TFooterColumn then
  begin
    Alignment := TFooterColumn(Source).Alignment;
    Bevel := TFooterColumn(Source).Bevel;
    BiDiMode := TFooterColumn(Source).BiDiMode;
    DisplayMask := TFooterColumn(Source).DisplayMask;
    FieldName := TFooterColumn(Source).FieldName;
    ParentBiDiMode := TFooterColumn(Source).ParentBiDiMode;
    Style := TFooterColumn(Source).Style;
    WidthIfIgnore := TFooterColumn(Source).WidthIfIgnore;
  end
  else
    inherited Assign(Source);
end;

function TFooterColumn.GetDisplayName: string;
begin
  Result := FieldName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TFooterColumn.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value Then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TFooterColumn.SetBevel(Value: TStatusPanelBevel);
begin
  if FBevel <> Value Then
  begin
    FBevel := Value;
    Changed(False);
  end;
end;

procedure TFooterColumn.SetBiDiMode(Value: TBiDiMode);
begin
  if FBiDiMode <> Value Then
  begin
    FBiDiMode := Value;
    Changed(False);
  end;
end;

procedure TFooterColumn.SetDisplayMask(Value: string);
begin
  if FDisplayMask <> Value Then
  begin
    FDisplayMask := Value;
    Changed(False);
  end;
end;

procedure TFooterColumn.SetFieldName(Value: string);
begin
  if FFieldName <> Value Then
  begin
    FFieldName := Value;
    FCurrentValue := Null;
    Changed(False);
  end;
end;

procedure TFooterColumn.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value Then
  begin
    FParentBiDiMode := Value;
    Changed(False);
  end;
end;

procedure TFooterColumn.SetStyle(Value: TStatusPanelStyle);
begin
  if FStyle <> Value Then
  begin
    FStyle := Value;
    Changed(False);
  end;
end;

procedure TFooterColumn.SetWidthIfIgnore(Value: Integer);
begin
  if FWidthIfIgnore <> Value Then
  begin
    FWidthIfIgnore := Value;
    Changed(False);
  end;
end;

{ TFooterColumns }

constructor TFooterColumns.Create(FooterBar: TJvDBGridFooter);
begin
  inherited Create(TFooterColumn);
  FFooterBar := FooterBar;
end;

function TFooterColumns.Add: TFooterColumn;
begin
  Result := TFooterColumn(inherited Add);
end;

function TFooterColumns.GetOwner: TPersistent;
begin
  Result := FFooterBar;
end;

procedure TFooterColumns.Update(Item: TCollectionItem);
begin
  FFooterBar.DrawPanels;
end;

function TFooterColumns.GetItem(Index: Integer): TFooterColumn;
begin
  Result := TFooterColumn(inherited GetItem(Index));
end;

procedure TFooterColumns.SetItem(Index: Integer; Value: TFooterColumn);
begin
  inherited SetItem(Index, Value);
end;

{ TFooterDataLink }

constructor TFooterDataLink.Create(AFooter: TJvDBGridFooter);
begin
  inherited Create;
  FGridFooter := AFooter;
  VisualControl := True;
end;

destructor TFooterDataLink.Destroy;
begin
  FGridFooter := nil;
  inherited Destroy;
end;

procedure TFooterDataLink.DataSetScrolled(Distance: Integer);
begin
// Don't remove this empty procedure. It prevents DataSetChanged
// from recalculating the footer values for every cursor move.
end;

procedure TFooterDataLink.ActiveChanged;
begin
  DataSetChanged;
end;

procedure TFooterDataLink.DataSetChanged;
begin
  if FGridFooter <> nil then
    FGridFooter.ReCalc;
end;

procedure TFooterDataLink.LayoutChanged;
begin
  if FGridFooter <> nil then
    FGridFooter.DrawPanels;
end;

{ TJvDBGridFooter }

constructor TJvDBGridFooter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  FJvDBGridLayoutChangeLink := TJvDBGridLayoutChangeLink.Create;
  FJvDBGridLayoutChangeLink.OnChange := JvDBGridLayoutChanged;
  
  FColumns := TFooterColumns.Create(Self);
  FDataLink := TFooterDataLink.Create(Self);
  FDBGrid := nil;
  FIgnoreHorzScrolling := False;
  FIgnoreResizing := False;
  SizeGrip := False;
end;

destructor TJvDBGridFooter.Destroy;
begin
  if Assigned(FDBGrid) then
    FDBGrid.UnregisterLayoutChangeLink(FJvDBGridLayoutChangeLink);
  
  FJvDBGridLayoutChangeLink.Free;  
    
  FDataLink.Free;
  FDataLink := nil;
  FColumns.Free;
  inherited Destroy;
end;

procedure TJvDBGridFooter.SetColumns(Value: TFooterColumns);
begin
  FColumns.Assign(Value);
end;

function TJvDBGridFooter.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBGridFooter.SetDataSource(Value: TDataSource);
begin
  if Assigned(DBGrid) then
    if Value <> DBGrid.DataSource then
      Value := DBGrid.DataSource;
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TJvDBGridFooter.SetDBGrid(Value: TJvDBGrid);
begin
  if FDBGrid <> Value then
  begin
    if Assigned(FDBGrid) then
      FDBGrid.UnregisterLayoutChangeLink(FJvDBGridLayoutChangeLink);
      
    FDBGrid := Value;
    
    if Assigned(FDBGrid) then
    begin
      DataSource := FDBGrid.DataSource;
      FDBGrid.RegisterLayoutChangeLink(FJvDBGridLayoutChangeLink);
    end;
  end;
end;

procedure TJvDBGridFooter.SetIgnoreHorzScrolling(Value: Boolean);
begin
  if FIgnoreHorzScrolling <> Value then
  begin
    FIgnoreHorzScrolling := Value;
    DrawPanels;
  end;
end;

procedure TJvDBGridFooter.SetIgnoreResizing(Value: Boolean);
begin
  if FIgnoreResizing <> Value then
  begin
    FIgnoreResizing := Value;
    DrawPanels;
  end;
end;

procedure TJvDBGridFooter.JvDBGridLayoutChanged(Grid: TJvDBGrid; Kind: TJvDBGridLayoutChangeKind);
begin
  case Kind of
    lcSizeChanged:
      DrawPanels;
    lcTopLeftChanged:
      DrawPanels;
  end;
end;

procedure TJvDBGridFooter.ReCalc;
var
  C: Integer;
begin
  for C := 0 to Columns.Count - 1 do
    if FDataLink.Active and Assigned(OnCalculate) then
      OnCalculate(Self, Columns.Items[C].FieldName, Columns.Items[C].FCurrentValue)
    else
      Columns.Items[C].FCurrentValue := Null;
  DrawPanels;
end;

procedure TJvDBGridFooter.DrawPanels;

  procedure CreatePanel(const GCol: Integer);
  var
    C: Integer;
    Found: Boolean;
    CurrentValue: Variant;
    NewText: string;
  begin
    if DBGrid.Columns[GCol].Visible then
      with Panels.Add do
      begin
        Found := False;
        for C := 0 to Columns.Count - 1 do
          if AnsiSameText(Columns.Items[C].FieldName, DBGrid.Columns[GCol].FieldName) then
          begin
            Found := True;
            Alignment := Columns.Items[C].Alignment;
            Bevel := Columns.Items[C].Bevel;
            BiDiMode := Columns.Items[C].BiDiMode;
            ParentBiDiMode := Columns.Items[C].ParentBiDiMode;
            Style := Columns.Items[C].Style;
            CurrentValue := Columns.Items[C].FCurrentValue;
            if (CurrentValue = Null) and Assigned(OnCalculate) then
              OnCalculate(Self, Columns.Items[C].FieldName, CurrentValue);
            if CurrentValue = Null then
              Text := ''
            else
            if Trim(Columns.Items[C].DisplayMask) <> '' then
            begin
              case VarType(CurrentValue) of
                varSmallint,
                varInteger:
                  Text := Format(Columns.Items[C].DisplayMask, [Integer(CurrentValue)]);
                varSingle,
                varDouble:
                  Text := Format(Columns.Items[C].DisplayMask, [Double(CurrentValue)]);
                varCurrency:
                  Text := Format(Columns.Items[C].DisplayMask, [Currency(CurrentValue)]);
                varDate:
                  Text := Format(Columns.Items[C].DisplayMask, [TDateTime(CurrentValue)]);
              else
                Text := string(CurrentValue);
              end;
            end
            else
              Text := string(CurrentValue);
            if IgnoreResizing then
              Width := Columns.Items[C].WidthIfIgnore
            else
              Width := DBGrid.Columns[GCol].Width;
            if Assigned(OnDisplayText) then
            begin
              NewText := Text;
              OnDisplayText(Self, Columns.Items[C], CurrentValue, NewText);
              Text := NewText;
            end;
            Break;
          end;
        if not Found then
        begin
          Bevel := pbNone;
          Style := psText;
          Text := '';
          if IgnoreResizing then
            Width := 0
          else
            Width := DBGrid.Columns[GCol].Width;
        end;
        if dgColLines in DBGrid.Options then
          Width := Width + TDrawGrid(DBGrid).GridLineWidth;
      end;
  end;

var
  I,
  FirstPanel,
  LastPanel: Integer;
begin
  Panels.Clear;
  if Assigned(DBGrid) and not SimplePanel then
  begin
    Panels.BeginUpdate;
    try
      // Datasource checking
      if DataSource <> DBGrid.DataSource then
        DataSource := DBGrid.DataSource;

      // Indicator panel
      if dgIndicator in DBGrid.Options then
        with Panels.Add do
        begin
          Bevel := pbNone;
          Style := psText;
          Text := '';
          Width := IndicatorWidth;
          if dgColLines in DBGrid.Options then
            Width := Width + TDrawGrid(DBGrid).GridLineWidth;
        end;

      // Fixed cols panels
      for I := 0 to DBGrid.FixedCols - 1 do
        CreatePanel(I);

      // Movable cols panels
      if IgnoreHorzScrolling then
      begin
        FirstPanel := DBGrid.FixedCols;
        LastPanel := DBGrid.Columns.Count - 1;
      end
      else
      begin
        if dgIndicator in DBGrid.Options then
          FirstPanel := DBGrid.LeftCol - 1
        else
          FirstPanel := DBGrid.LeftCol;
        LastPanel := FirstPanel + DBGrid.VisibleColCount;
        if LastPanel >= DBGrid.Columns.Count then
          LastPanel := DBGrid.Columns.Count - 1;
      end;
      for I := FirstPanel to LastPanel do
        CreatePanel(I);

      // Ending panel
      with Panels.Add do
      begin
        Bevel := pbNone;
        Style := psText;
        Text := '';
        Width := 1;
      end;
    finally
      Panels.EndUpdate;
    end;
  end;
end;  

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.