unit glGHC;

interface
{$I glDEF.INC}
uses Windows, Classes, comctrls, grids, sysutils, Forms, dbgrids, dialogs;

type
  TglGridHeaderControl = class(THeaderControl)
  private
    FGrid: TCustomGrid;
    FJoinColumns: TStringList;
//    aColWidths: array[0..255] of word;
    FEqualSize: boolean;
    FSections: THeaderSections;
  public
    FActiveSectionNo: integer;
    procedure ResizeColumns;
  protected
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetSections(Value: THeaderSections);
    procedure SetJoinColumns(Value: TStringList);
    procedure SetEqualSize(Value: boolean);
    procedure SectionResize(Section: THeaderSection); override;
  published
    property Grid: TCustomGrid read FGrid write FGrid;
    property JoinColumns: TStringList read FJoinColumns write SetJoinColumns;
    property EqualSize: boolean read FEqualSize write SetEqualSize default true;
//    property Sections: THeaderSections read FSections write SetSections;
  end;

  TPublicGrid = class(TCustomGrid)
  end;

implementation
uses glUtils;

constructor TglGridHeaderControl.Create(AOwner: TComponent);
begin
  inherited;
  FJoinColumns := TStringList.Create;
  FSections := THeaderSections.Create(self);
  FEqualSize := true;
end;

destructor TglGridHeaderControl.Destroy;
begin
  FJoinColumns.Free;
  inherited;
end;

procedure TglGridHeaderControl.Loaded;
var i: integer;
begin
  inherited;
  if not Assigned(Grid) then exit;
end;

procedure TglGridHeaderControl.Resize;
var i: integer;
begin
  inherited;
  if not Assigned(Grid) then exit;
//  for i:=0 to TPublicGrid(Grid).ColCount-1 do
//    aColWidths[i] := TPublicGrid(Grid).ColWidths[i];

  ResizeColumns;
end;

procedure TglGridHeaderControl.SectionResize(Section: THeaderSection);
begin
  inherited;
  ResizeColumns;
end;

procedure TglGridHeaderControl.ResizeColumns;
var
  i, ItemsCount, Col, Sect, ColsToJoin, ColsToJoinWidth: integer;
  fIndicator: boolean;
  G: TPublicGrid;
begin
  if not Assigned(Grid) then exit;

  G := TPublicGrid(Grid);
  ItemsCount := min(G.ColCount, Sections.Count);
  for i:=0 to max(FJoinColumns.Count-1, Sections.Count-1) do
    try
      if FJoinColumns.Count <= i then FJoinColumns.Add('1');
      FJoinColumns.Objects[i] := Pointer(StrToInt(FJoinColumns[i]));
    except
      FJoinColumns.Objects[i] := Pointer(1);
    end;

  Col := 0; Sect := 0;
  fIndicator := (Grid is TDBGrid)and(dgIndicator in TDBGrid(g).Options);
  if fIndicator then col := 1;

  while (Col < G.ColCount)and(Sect < Sections.Count) do
  begin
    ColsToJoin := min(integer(FJoinColumns.Objects[Sect]), G.ColCount - Col);

    ColsToJoinWidth := 0;
    for i:=0 to ColsToJoin-1 do
      inc(ColsToJoinWidth, g.ColWidths[Col+i]);

//      inc(ColsToJoinWidth ,-ColsToJoin);

//    ColsToJoinWidth := 0;
//    for i:=0 to ColsToJoin-1 do
//      inc(ColsToJoinWidth, G.ColWidths[Col+i]);

    if ColsToJoinWidth <> 0 then
    begin
      for i:=0 to ColsToJoin-1 do
      begin
        if EqualSize then
          G.ColWidths[Col+i] := trunc((ColsToJoinWidth / ColsToJoin / ColsToJoinWidth)*Sections[Sect].Width)-1
        else
          G.ColWidths[Col+i] := trunc((g.ColWidths[Col+i] / ColsToJoinWidth)*Sections[Sect].Width)-1;
      end;
      //G.ColWidths[Col + ColsToJoin-1] := G.ColWidths[Col + ColsToJoin-1] + Sections[Sect].Width - ColsToJoinWidth - ColsToJoin;
    end;

    inc(Col, integer(JoinColumns.Objects[Sect]));
    inc(Sect);
  end;
  if G.BorderStyle <> bsNone then
    G.ColWidths[integer(fIndicator)] := G.ColWidths[integer(fIndicator)] - 3;
  if fIndicator then
    G.ColWidths[1] := G.ColWidths[1] - 12;
end;
//------------------------------------------------------------------------------

procedure TglGridHeaderControl.SetSections(Value: THeaderSections);
begin
  FSections.Assign(Value);
end;

procedure TglGridHeaderControl.SetJoinColumns(Value: TStringList);
begin
  FJoinColumns.Assign(Value);
  ResizeColumns;
end;

procedure TglGridHeaderControl.SetEqualSize(Value: boolean);
begin
  FEqualSize := Value;
  ResizeColumns;
end;

end.
