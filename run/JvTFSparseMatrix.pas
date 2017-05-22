{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTFSparseMatrix.PAS, released on 2003-08-01.

The Initial Developer of the Original Code is Unlimited Intelligence Limited.
Portions created by Unlimited Intelligence Limited are Copyright (C) 1999-2002 Unlimited Intelligence Limited.
All Rights Reserved.

Contributor(s):
Mike Kolter (original code)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTFSparseMatrix;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, SysUtils;

type
  {$IFNDEF COMPILER12_UP}
  NativeInt = Integer;
  {$ENDIF ~COMPILER12_UP}

  EJvTFSparseMatrixError = class(Exception);
  PSMQuantum = ^TSMQuantum;
  TSMQuantum = record
    Index: Integer;
    Data: NativeInt;
    Link: PSMQuantum;
  end;

  TJvTFSparseMatrix = class(TObject)
  private
    FMatrix: TSMQuantum;
    FNullValue: NativeInt;
    procedure SetNullValue(Value: NativeInt);
    function GetData(Row, Col: Integer): NativeInt;
    procedure SetData(Row, Col: Integer; Value: NativeInt);
    procedure Put(Row, Col: Integer; Data: NativeInt);
    function Get(Row, Col: Integer): NativeInt;
    function FindQuantum(Row, Col: Integer;
      var Prev, Curr: PSMQuantum; var RowExists: Boolean): Boolean;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Pack;
    procedure CopyTo(DestMatrix: TJvTFSparseMatrix);
    property Data[Row, Col: Integer]: NativeInt read GetData write SetData; default;
    property NullValue: NativeInt read FNullValue write SetNullValue default 0;
    procedure Dump(const DumpList: TStrings);
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
  JvResources;

destructor TJvTFSparseMatrix.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJvTFSparseMatrix.Clear;
var
  P, CurrRow, CurrCol: PSMQuantum;
begin
  CurrRow := PSMQuantum(FMatrix.Data);

  while CurrRow <> nil do
  begin
    CurrCol := CurrRow^.Link;
    while CurrCol <> nil do
    begin
      P := CurrCol;
      CurrCol := CurrCol^.Link;
      Dispose(P);
    end;

    P := CurrRow;
    CurrRow := PSMQuantum(CurrRow^.Data);
    Dispose(P);
  end;

  FMatrix.Data := 0;
end;

procedure TJvTFSparseMatrix.CopyTo(DestMatrix: TJvTFSparseMatrix);
var
  CurrRow, CurrCol: PSMQuantum;
begin
  DestMatrix.Clear;
  DestMatrix.NullValue := NullValue;

  CurrRow := PSMQuantum(FMatrix.Data);

  while CurrRow <> nil do
  begin
    CurrCol := CurrRow^.Link;
    while CurrCol <> nil do
    begin
      DestMatrix[CurrRow^.Index, CurrCol^.Index] := CurrCol^.Data;
      CurrCol := CurrCol^.Link;
    end;

    CurrRow := PSMQuantum(CurrRow^.Data);
  end;
end;

procedure TJvTFSparseMatrix.Dump(const DumpList: TStrings);
var
  CurrRow, CurrCol: PSMQuantum;
begin
  DumpList.Clear;
  CurrRow := PSMQuantum(FMatrix.Data);
  DumpList.BeginUpdate;
  try
    while CurrRow <> nil do
    begin
      CurrCol := CurrRow^.Link;
      while CurrCol <> nil do
      begin
        DumpList.Add('(' + IntToStr(CurrRow^.Index) + ', ' +
          IntToStr(CurrCol^.Index) + ') ' +
          IntToStr(CurrCol^.Data));
        CurrCol := CurrCol^.Link;
      end;
      CurrRow := PSMQuantum(CurrRow^.Data);
    end;
  finally
    DumpList.EndUpdate;
  end;
end;

function TJvTFSparseMatrix.FindQuantum(Row, Col: Integer;
  var Prev, Curr: PSMQuantum; var RowExists: Boolean): Boolean;
begin
  Prev := @FMatrix;
  Curr := PSMQuantum(FMatrix.Data);
  Result := False;
  RowExists := False;

  // Find Row Header
  while (Curr <> nil) and (Curr^.Index < Row) do
  begin
    Prev := Curr;
    Curr := PSMQuantum(Curr^.Data);
  end;

  // If Row Header found, then find col
  if (Curr <> nil) and (Curr^.Index = Row) then
  begin
    RowExists := True;
    Prev := Curr;
    Curr := Curr^.Link;
    while (Curr <> nil) and (Curr^.Index < Col) do
    begin
      Prev := Curr;
      Curr := Curr^.Link;
    end;

    Result := (Curr <> nil) and (Curr^.Index = Col);
  end;
end;

function TJvTFSparseMatrix.Get(Row, Col: Integer): NativeInt;
var
  Prev, Curr: PSMQuantum;
  RowExists: Boolean;
begin
  if FindQuantum(Row, Col, Prev, Curr, RowExists) then
    Result := Curr^.Data
  else
    Result := NullValue;
end;

function TJvTFSparseMatrix.GetData(Row, Col: Integer): NativeInt;
begin
  Result := Get(Row, Col);
end;

procedure TJvTFSparseMatrix.Put(Row, Col: Integer; Data: NativeInt);
var
  P, Prev, Curr: PSMQuantum;
  RowExists: Boolean;
begin
  if FindQuantum(Row, Col, Prev, Curr, RowExists) then
    if Data <> NullValue then
      Curr^.Data := Data
    else
    begin
      Prev^.Link := Curr^.Link;
      Dispose(Curr);
    end
  else
  if Data <> NullValue then
  begin
    if not RowExists then
    begin
      New(P);
      P^.Index := Row;
      P^.Link := nil;
      P^.Data := Prev^.Data;
      PSMQuantum(Prev^.Data) := P;
      Prev := P;
    end;

    New(P);
    P^.Index := Col;
    P^.Data := Data;
    P^.Link := Prev^.Link;
    Prev^.Link := P;
  end;
end;

procedure TJvTFSparseMatrix.SetData(Row, Col: Integer; Value: NativeInt);
begin
  Put(Row, Col, Value);
end;

procedure TJvTFSparseMatrix.SetNullValue(Value: NativeInt);
begin
  if FMatrix.Data = 0 then
    FNullValue := Value
  else
    raise EJvTFSparseMatrixError.CreateRes(@RsEMatrixMustBeEmpty);
end;

procedure TJvTFSparseMatrix.Pack;
var
  P, Prev, CurrRow: PSMQuantum;
begin
  CurrRow := PSMQuantum(FMatrix.Data);
  Prev := @FMatrix;

  while CurrRow <> nil do
  begin
    if CurrRow^.Link <> nil then
    begin
      Prev := CurrRow;
      CurrRow := PSMQuantum(CurrRow^.Data);
    end
    else
    begin
      P := CurrRow;
      Prev^.Data := CurrRow^.Data;
      Dispose(P);
      CurrRow := PSMQuantum(Prev^.Data);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
