{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

unit transaction;

{$I jvcl.inc}
{$IFDEF COMPILER7_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}


interface

uses
  ComObj, ActiveX, UIB_TLB, StdVcl, JvUIB, InternalItf;

type

  TOleTransaction = class(TJvUIBTransaction)
  private
    FDatabase: IDatabase;
  protected
    procedure SetDataBase(const Database: TJvUIBDataBase); override;
  end;

  TTransaction = class(TAutoObject, ITransaction, IDataPointer)
  private
    FTransaction: TOleTransaction;
  protected
    function Data: pointer; stdcall;
    function Get_Database: IDatabase; safecall;
    procedure Set_Database(const Value: IDatabase); safecall;
    procedure Commit; safecall;
    function Get_InTransaction: WordBool; safecall;
    procedure CommitRetaining; safecall;
    procedure RollBack; safecall;
    procedure RollBackRetaining; safecall;

  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

{ TOleTransaction }

procedure TOleTransaction.SetDataBase(const Database: TJvUIBDataBase);
begin
  inherited;
  if (DataBase = nil) then FDatabase := nil; // if database disconnect transaction
end;

{ TTransaction }

destructor TTransaction.Destroy;
begin
  FTransaction.Free;
  inherited;
end;

procedure TTransaction.Initialize;
begin
  inherited;
  FTransaction := TOleTransaction.Create(nil);
end;

function TTransaction.Get_Database: IDatabase;
begin
  Result := FTransaction.FDatabase;
end;

procedure TTransaction.Set_Database(const Value: IDatabase);
begin
  if (Value = nil) then
    FTransaction.SetDataBase(nil) else
    FTransaction.SetDataBase((value as IDataPointer).Data);
end;

function TTransaction.Data: pointer;
begin
  Result := FTransaction;
end;

procedure TTransaction.Commit;
begin
  FTransaction.Commit;
end;

function TTransaction.Get_InTransaction: WordBool;
begin
  Result := FTransaction.InTransaction;
end;

procedure TTransaction.CommitRetaining;
begin
  FTransaction.CommitRetaining;
end;

procedure TTransaction.RollBack;
begin
  FTransaction.RollBack;
end;

procedure TTransaction.RollBackRetaining;
begin
  FTransaction.RollBackRetaining;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TTransaction, Class_Transaction,
    ciMultiInstance, tmApartment);
end.
