{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGridFilter.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  When Position 0 you can not click on the far left of the button to move.
  When Position 100 you can not click on the far Right of the button to move.

-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvGridFilter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids;

type
  TJvGridFilter = class(TComponent)
  private
    FGrid: TStringGrid;
    procedure ApplyFilter;

    function parseFilter(Afilter: string): boolean;
    procedure SetGrid(const Value: TStringGrid);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Filter(AFilter: string);
    procedure ShowRows;
  published
    { Published declarations }
    property Grid: TStringGrid read FGrid write SetGrid;
  end;

implementation

type
  TGridFilterFunc = function(FieldValue, FilterValue: string): boolean;

  TGridFieldFilter = record
    FilterFunc: TGridFilterFunc;
    FilterField: integer;
    FilterValue: string;
  end;

  TGridRowFilter = record
    FilterCount: integer;
    Filters: array[0..9] of TGridFieldFilter;
  end;

var
  GridRowFilter: TGridRowFilter;

  // Grid filter functions

function filterEQ(FieldValue, FilterValue: string): boolean;
begin
  result := FieldValue = FilterValue;
end;

function filterNE(FieldValue, FilterValue: string): boolean;
begin
  result := FieldValue <> FilterValue;
end;

function filterGT(FieldValue, FilterValue: string): boolean;
begin
  result := FieldValue > FilterValue;
end;

function filterLT(FieldValue, FilterValue: string): boolean;
begin
  result := FieldValue < FilterValue;
end;

function filterLIKE(FieldValue, FilterValue: string): boolean;
begin
  result := pos(lowercase(FilterValue), lowercase(FieldValue)) > 0;
end;

function TJvGridFilter.parseFilter(Afilter: string): boolean;
var
  op, s: string;
  f: TGridFilterFunc;
  fieldnr, i, p: integer;
  Fieldname, Filtervalue: string;
begin
  result := false;
  GridRowFilter.FilterCount := 0;
  s := trim(Afilter);
  if s = '' then exit;
  // parse field name
  repeat
    p := pos('[', s);

    if p = 0 then exit;
    s := copy(s, p + 1, length(s));
    p := pos(']', s);
    if p = 0 then exit;
    Fieldname := copy(s, 1, p - 1);
    s := trim(copy(s, p + 1, length(s)));
    if FieldName = '' then exit;
    // find fieldnumber
    FieldNr := 0;
    for i := 1 to Grid.colcount - 1 do
      if Grid.cells[i, 0] = Fieldname then
      begin
        Fieldnr := i;
        Break;
      end;
    if fieldnr = 0 then exit;
    // we have the field number, now check operand
    p := pos('"', s); // " marks the beginning of the filter value
    if p = 0 then exit;
    op := lowercase(trim(copy(s, 1, p - 1)));
    s := copy(s, p + 1, length(s));
    p := pos('"', s); // find the end of the filtervalue
    if p = 0 then exit;
    FilterValue := copy(s, 1, p - 1);
    s := trim(copy(s, p + 1, length(s)));
    if op = '=' then
      f := filterEQ
    else if op = '<>' then
      f := filterNE
    else if op = '>' then
      f := filterGT
    else if op = '<' then
      f := filterLT
    else if op = 'like' then
      f := filterLIKE
    else
      exit;
    inc(GridRowFilter.FilterCount);
    if (GridRowFilter.FilterCount > 9) then
    begin
      showmessage('Filter too complex');
      GridRowFilter.FilterCount := 0;
      exit;
    end;
    GridrowFilter.Filters[Gridrowfilter.FilterCount - 1].FilterFunc := f; //invalid warning
    GridrowFilter.Filters[Gridrowfilter.FilterCount - 1].FilterField := FieldNr;
    GridrowFilter.Filters[Gridrowfilter.FilterCount - 1].FilterValue := FilterValue;
  until s = '';
  result := true;
end;

procedure TJvGridFilter.ApplyFilter;
var
  arow, fi, fc, Filterfield: integer;
  FieldValue, FilterValue: string;
  fn: TGridFilterFunc;
  CanHide: boolean;
begin
  if GridRowFilter.FilterCount = 0 then exit;
  fc := GridRowFilter.FilterCount;
  for arow := 1 to Grid.rowcount - 1 do
  begin
    CanHide := false;
    for fi := 0 to fc - 1 do
    begin
      fn := GridRowFilter.Filters[fi].FilterFunc;
      FilterValue := GridRowFilter.Filters[fi].FilterValue;
      filterField := GridRowFilter.Filters[fi].FilterField;
      FieldValue := Grid.cells[FilterField, arow];
      if not fn(FieldValue, FilterValue) then
      begin
        CanHide := true;
        break;
      end;
    end;
    if CanHide then Grid.rowheights[arow] := 0;
  end;
end;

procedure TJvGridFilter.Filter(AFilter: string);
begin
  if assigned(FGrid) then
    if parseFilter(AFilter) then
      ApplyFilter;
end;

procedure TJvGridFilter.SetGrid(const Value: TStringGrid);
begin
  FGrid := Value;
end;

procedure TJvGridFilter.ShowRows;
var
  arow: integer;
begin
  for arow := 0 to Grid.rowcount - 1 do
    Grid.rowheights[arow] := grid.defaultrowheight;
end;

end.
