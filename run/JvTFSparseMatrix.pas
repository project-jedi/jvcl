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

Last Modified: 2003-08-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTFSparseMatrix;

interface

uses Classes, SysUtils;

Type
  EJvTFSparseMatrixError = class(Exception);
  PSMQuantum = ^TSMQuantum;
  TSMQuantum = record
    Index : Integer;
    Data : Integer;
    Link : PSMQuantum;
  End;

  TJvTFSparseMatrix = class
  private
    FMatrix : TSMQuantum;
    FNullValue : Integer;
    procedure SetNullValue(Value : Integer);
    function GetData(Row, Col : Integer) : Integer;
    procedure SetData(Row, Col, Value : Integer);
    procedure Put(Row, Col, Data : Integer);
    function Get(Row, Col : Integer) : Integer;
    procedure FindQuantum(Row, Col : Integer;
                          var Prev, Curr : PSMQuantum;
                          var RowExists, Found : Boolean);
  public
    Destructor Destroy; override;
    procedure Clear;
    procedure Pack;
    procedure CopyTo(DestMatrix : TJvTFSparseMatrix);
    property Data[Row, Col : Integer] : Integer read GetData write SetData; default;
    property NullValue : Integer read FNullValue write SetNullValue default 0;
    procedure Dump(DumpList : TStringList);
  end;

implementation

{ TJvTFSparseMatrix }

Destructor TJvTFSparseMatrix.Destroy;
Begin
  Clear;
  Inherited;
End;

procedure TJvTFSparseMatrix.Clear;
var
  P,
  CurrRow,
  CurrCol : PSMQuantum;
begin
  CurrRow := PSMQuantum(FMatrix.Data);

  While CurrRow <> nil do
    Begin
      CurrCol := CurrRow^.Link;
      While CurrCol <> nil do
        Begin
          P := CurrCol;
          CurrCol := CurrCol^.Link;
          Dispose(P);
        End;

      P := CurrRow;
      CurrRow := PSMQuantum(CurrRow^.Data);
      Dispose(P);
    End;

  FMatrix.Data := 0;
end;

procedure TJvTFSparseMatrix.CopyTo(DestMatrix: TJvTFSparseMatrix);
var
  CurrRow,
  CurrCol : PSMQuantum;
begin
  DestMatrix.Clear;
  DestMatrix.NullValue := NullValue;

  CurrRow := PSMQuantum(FMatrix.Data);

  While CurrRow <> nil do
    Begin
      CurrCol := CurrRow^.Link;
      While CurrCol <> nil do
        Begin
          DestMatrix[CurrRow^.Index, CurrCol^.Index] := CurrCol^.Data;
          CurrCol := CurrCol^.Link;
        End;

      CurrRow := PSMQuantum(CurrRow^.Data);
    End;
end;

procedure TJvTFSparseMatrix.Dump(DumpList : TStringList);
var
  CurrRow,
  CurrCol : PSMQuantum;
begin
  DumpList.Clear;

  CurrRow := PSMQuantum(FMatrix.Data);

  While CurrRow <> nil do
    Begin
      CurrCol := CurrRow^.Link;
      While CurrCol <> nil do
        Begin
          DumpList.Add('(' + IntToStr(CurrRow^.Index) + ', ' +
                       IntToStr(CurrCol^.Index) + ') ' +
                       IntToStr(CurrCol^.Data));
          CurrCol := CurrCol^.Link;
        End;

      Currrow := PSMQuantum(CurrRow^.Data);
    End;
end;

procedure TJvTFSparseMatrix.FindQuantum(Row, Col: Integer; var Prev,
  Curr: PSMQuantum; var RowExists, Found: Boolean);
begin
  Prev := @FMatrix;
  Curr := PSMQuantum(FMatrix.Data);
  Found := False;
  RowExists := False;

  // Find Row Header
  While (Curr <> nil) and (Curr^.Index < Row) do
    Begin
      Prev := Curr;
      Curr := PSMQuantum(Curr^.Data);
    End;

  // If Row Header found, then find col
  If (Curr <> nil) and (Curr^.Index = Row) Then
    Begin
      RowExists := True;
      Prev := Curr;
      Curr := Curr^.Link;
      While (Curr <> nil) and (Curr^.Index < Col) do
        Begin
          Prev := Curr;
          Curr := Curr^.Link;
        End;

      // If Col found, then set Found to True
      If (Curr <> nil) and (Curr^.Index = Col) Then
        Found := True;
    End;
end;

function TJvTFSparseMatrix.Get(Row, Col: Integer): Integer;
var
  Prev,
  Curr : PSMQuantum;
  RowExists,
  Found : Boolean;
begin
  FindQuantum(Row, Col, Prev, Curr, RowExists, Found);

  If Found Then
    Result := Curr^.Data
  Else
    Result := NullValue;
end;

function TJvTFSparseMatrix.GetData(Row, Col: Integer): Integer;
begin
  Result := Get(Row, Col);
end;

procedure TJvTFSparseMatrix.Put(Row, Col, Data: Integer);
var
  P,
  Prev,
  Curr : PSMQuantum;
  RowExists,
  Found : Boolean;
begin
  FindQuantum(Row, Col, Prev, Curr, RowExists, Found);

  If Found Then
    If Data <> NullValue Then
      Curr^.Data := Data
    Else
      Begin
        Prev^.Link := Curr^.Link;
        Dispose(Curr);
      End
  Else
    If Data <> NullValue Then
      Begin
        If not RowExists Then
          Begin
            New(P);
            P^.Index := Row;
            P^.Link := nil;
            P^.Data := Prev^.Data;
            PSMQuantum(Prev^.Data) := P;
            Prev := P;
          End;

        New(P);
        P^.Index := Col;
        P^.Data := Data;
        P^.Link := Prev^.Link;
        Prev^.Link := P;
      End;
end;

procedure TJvTFSparseMatrix.SetData(Row, Col, Value: Integer);
begin
  Put(Row, Col, Value);
end;

procedure TJvTFSparseMatrix.SetNullValue(Value: Integer);
begin
  If FMatrix.Data = 0 Then
    FNullValue := Value
  Else
    Raise EJvTFSparseMatrixError.Create('Matrix must be empty before setting null value');
end;

procedure TJvTFSparseMatrix.Pack;
var
  P,
  Prev,
  CurrRow : PSMQuantum;
begin
  CurrRow := PSMQuantum(FMatrix.Data);
  Prev := @FMatrix;

  While CurrRow <> nil do
    Begin
      If CurrRow^.Link <> nil Then
        Begin
          Prev := CurrRow;
          CurrRow := PSMQuantum(CurrRow^.Data);
        End
      Else
        Begin
          P := CurrRow;
          Prev^.Data := CurrRow^.Data;
          Dispose(P);
          CurrRow := PSMQuantum(Prev^.Data);
        End;
    End;
end;

end.
