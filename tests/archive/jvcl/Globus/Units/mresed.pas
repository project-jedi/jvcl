unit mresed;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, dsgnintf,
  StdCtrls, glMRes, Grids;

type

  TglResourcesProperty = class( TPropertyEditor )
    function GetAttributes : TPropertyAttributes; override;
    function GetValue : string; override;
    procedure Edit; override;
  end;

  TglMresEdit = class(TForm)
    sg: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure sgSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ControlsList: TList;
    procedure LoadDefaults;
  public
    Component: TglMultiResources;
    function GetSub( SrcStr: string; No: integer; var ResStr: string): boolean;
  end;

var
  glMresEdit: TglMresEdit;

implementation
{$R *.DFM}


function TglResourcesProperty.GetAttributes : TPropertyAttributes;
begin
  Result := [ paDialog ];
end;

function TglResourcesProperty.GetValue : string;
begin
  Result := Format( '(%s)', [ GetPropType^.Name ] );
end;

procedure TglResourcesProperty.Edit;
var
  Dialog : TglMresEdit;
  I : Integer;
begin
  TglMultiResources(GetComponent(0)).Update;
  Dialog := glMresEdit.Create( Application );
  Dialog.Component := TglMultiResources(GetComponent(0));
  Dialog.ShowModal;
  Dialog.free;
//  GetComponent(0).Owner.Name
end;

procedure TglMresEdit.LoadDefaults;
var ARow: integer;
begin
  sg.RowCount := Component.Comps.Count+2;
  for ARow := 1 to Component.Comps.Count do
    sg.Cells[1,ARow] := Component.Comps[ARow-1];
end;

procedure TglMresEdit.FormShow(Sender: TObject);
var
  Str, SubStr, ResStr: string;
  i, uPos1, uPos2, ACol, ARow, SubStrNo: integer;
begin

  sg.ColCount := 3;
  sg.RowCount := Component.Resources.Count+2;
  sg.Cells[0,0] := 'Control';
  sg.Cells[1,0] := 'Default';
  LoadDefaults;
  with Component, sg do
  for ARow := 1 to Resources.Count do
  begin
    Str := Resources[ARow-1];
    uPos1 := 1;
    uPos2 := uPos1+1;
    ACol := 0;
    SubStrNo := 1;
    while GetSub( Str, SubStrNo, ResStr) do
    begin
      inc(ACol); inc(SubStrNo);
      if sg.ColCount < ACol+1 then sg.ColCount := ACol+1;
      Cells[ ACol, ARow ] := ResStr;
    end;
    Cells[ 0, ARow ] := Component.Comps[ARow-1];
{    repeat
      if Str[uPos2] = '#' then
      begin
	Cells[ ACol, ARow ] := copy(str, uPos1, uPos2-uPos1 );
	inc(uPos2);
	uPos1 := uPos2;
	inc(ACol);
      end;
      inc(uPos2);
    until uPos2 >= length(Str);}

//    Cells[ ACol, ARow ] := copy(str, uPos1, uPos2-uPos1+1 );

  end;
  sg.FixedCols := 1; sg.FixedRows := 1;
end;

function TglMresEdit.GetSub( SrcStr: string; No: integer; var ResStr: string): boolean;
var
  Str, SubStr: string;
  Counter, uPos1, uPos2, uPrevPos2, ACol, ARow: integer;
begin
  uPos1:=1; uPos2:=1; uPrevPos2 := 1;
  Counter := 0;
  ResStr := '';
  if SrcStr='' then exit;
  repeat
    if SrcStr[uPos2] = '#' then
    begin inc(Counter); uPos1 := uPrevPos2; uPrevPos2 := uPos2; end;
    inc(uPos2);
  until (Counter = No)or(uPos2 = length(SrcStr));
  Result := true;
  if Counter = No then ResStr := copy(SrcStr, uPos1-1, uPos2-uPos1-1 ) else
  if Counter+1 = No then ResStr := copy(SrcStr, uPrevPos2, uPos2-uPrevPos2+1 )
		  else Result := false;

end;

procedure TglMresEdit.sgSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: String);
begin
  if ACol<>1 then exit;
  //if Tlabel(ControlsList[ARow-1]).Caption <> Value then
//    Tlabel(ControlsList[ARow-1]).Caption := Value;
end;

procedure TglMresEdit.FormCreate(Sender: TObject);
begin
  ControlsList := TList.create;
end;

procedure TglMresEdit.FormDestroy(Sender: TObject);
begin
  ControlsList.Free;
end;

end.
