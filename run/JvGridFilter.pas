{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGridFilter.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  When Position 0 you can not click on the far left of the button to move.
  When Position 100 you can not click on the far right of the button to move.

-----------------------------------------------------------------------------}
// $Id$

unit JvGridFilter;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Graphics, Controls, Forms, Grids,
  SysUtils, Classes;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvGridFilter = class(TComponent)
  private
    FGrid: TStringGrid;
    FGridRowFilter: TList;
    procedure ApplyFilter;
    function ParseFilter(const AFilter: string): Boolean;
    procedure SetGrid(const Value: TStringGrid);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Filter(const AFilter: string);
    procedure ShowRows;
  published
    property Grid: TStringGrid read FGrid write SetGrid;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvConsts;

type
  TGridFilterFunc = function(const FieldValue, FilterValue: string): Boolean;

  PGridFieldFilter = ^TGridFieldFilter;
  TGridFieldFilter = record
    FilterFunc: TGridFilterFunc;
    FilterField: Integer;
    FilterValue: string;
  end;

function FilterEQ(const FieldValue, FilterValue: string): Boolean;
begin
  Result := FieldValue = FilterValue;
end;

function FilterNE(const FieldValue, FilterValue: string): Boolean;
begin
  Result := FieldValue <> FilterValue;
end;

function FilterGT(const FieldValue, FilterValue: string): Boolean;
begin
  Result := FieldValue > FilterValue;
end;

function FilterLT(const FieldValue, FilterValue: string): Boolean;
begin
  Result := FieldValue < FilterValue;
end;

function FilterLIKE(const FieldValue, FilterValue: string): Boolean;
begin
  Result := Pos(LowerCase(FilterValue), LowerCase(FieldValue)) > 0;
end;

constructor TJvGridFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGridRowFilter := TList.Create;
end;

destructor TJvGridFilter.Destroy;
var
  I: Integer;
begin
  for I := 0 to FGridRowFilter.Count-1 do
    Dispose(FGridRowFilter[I]);
  FGridRowFilter.Free;
  inherited Destroy;
end;

function TJvGridFilter.ParseFilter(const AFilter: string): Boolean;
var
  Op, S: string;
  Func: TGridFilterFunc;
  FieldNr, I, P: Integer;
  FieldName, FilterValue: string;
  Filt: PGridFieldFilter;
begin
  Result := False;
  for I := 0 to FGridRowFilter.Count-1 do
    Dispose(FGridRowFilter[I]);
  FGridRowFilter.Clear;

  S := Trim(AFilter);
  if S = '' then
    Exit;
  {$IFNDEF CPUX64}
  @Func := nil;
  {$ENDIF ~CPUX64}
  // parse field name
  repeat
    P := Pos('[', S);

    if P = 0 then
      Exit;
    S := Copy(S, P + 1, Length(S));
    P := Pos(']', S);
    if P = 0 then
      Exit;
    FieldName := Copy(S, 1, P - 1);
    S := Trim(Copy(S, P + 1, Length(S)));
    if FieldName = '' then
      Exit;
    // find fieldnumber
    FieldNr := 0;
    for I := 1 to Grid.ColCount - 1 do
      if Grid.Cells[I, 0] = FieldName then
      begin
        FieldNr := I;
        Break;
      end;
    if FieldNr = 0 then
      Exit;
    // we have the field number, now check operand
    P := Pos('"', S); // " marks the beginning of the filter value
    if P = 0 then
      Exit;
    Op := LowerCase(Trim(Copy(S, 1, P - 1)));
    S := Copy(S, P + 1, Length(S));
    P := Pos('"', S); // find the end of the FilterValue
    if P = 0 then
      Exit;
    FilterValue := Copy(S, 1, P - 1);
    S := Trim(Copy(S, P + 1, Length(S)));
//    Func := nil;
    if Op = '=' then
      Func := FilterEQ
    else
    if Op = '<>' then
      Func := FilterNE
    else
    if Op = '>' then
      Func := FilterGT
    else
    if Op = '<' then
      Func := FilterLT
    else
    if Op = 'like' then
      Func := FilterLIKE
    else
      Exit;

    New(Filt);
    Filt^.FilterFunc := Func;
    Filt^.FilterField := FieldNr;
    Filt^.FilterValue := FilterValue;
    FGridRowFilter.Add(Filt);
  until S = '';
  Result := True;
end;

procedure TJvGridFilter.ApplyFilter;
var
  Row, I: Integer;
  FieldValue: string;
  CanHide: Boolean;
  Filt: PGridFieldFilter;
begin
  if FGridRowFilter.Count = 0 then
    Exit;
  for Row := 1 to Grid.RowCount - 1 do
  begin
    CanHide := False;
    for I := 0 to FGridRowFilter.Count - 1 do
    begin
      Filt := FGridRowFilter[I];
      FieldValue := Grid.Cells[Filt^.FilterField, Row];
      if not Filt^.FilterFunc(FieldValue, Filt^.FilterValue) then
      begin
        CanHide := True;
        Break;
      end;
    end;
    if CanHide then
      Grid.RowHeights[Row] := 0;
  end;
end;

procedure TJvGridFilter.Filter(const AFilter: string);
begin
  if Assigned(FGrid) then
    if ParseFilter(AFilter) then
      ApplyFilter;
end;

procedure TJvGridFilter.SetGrid(const Value: TStringGrid);
begin
  FGrid := Value;
end;

procedure TJvGridFilter.ShowRows;
var
  Row: Integer;
begin
  for Row := 0 to Grid.RowCount - 1 do
    Grid.RowHeights[Row] := Grid.DefaultRowHeight;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.