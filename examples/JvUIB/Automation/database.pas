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

unit database;
{$I jvcl.inc}
{$IFDEF COMPILER7_UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  ComObj, ActiveX, UIB_TLB, JvUIB, InternalItf, StdVcl;

type

  TDatabase = class(TAutoObject, IDatabase, IDataPointer)
  private
    FDatabase: TJvUIBDataBase;
  protected
    function Data: pointer; stdcall;
    function Get_CharacterSet: WideString; safecall;
    function Get_Connected: WordBool; safecall;
    function Get_DatabaseName: WideString; safecall;
    function Get_PassWord: WideString; safecall;
    function Get_SQLDialect: Integer; safecall;
    function Get_UserName: WideString; safecall;
    procedure Set_CharacterSet(const Value: WideString); safecall;
    procedure Set_Connected(Value: WordBool); safecall;
    procedure Set_DatabaseName(const Value: WideString); safecall;
    procedure Set_PassWord(const Value: WideString); safecall;
    procedure Set_SQLDialect(Value: Integer); safecall;
    procedure Set_UserName(const Value: WideString); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

{ TDatabase }

function TDatabase.Data: pointer;
begin
  result := FDatabase;
end;

destructor TDatabase.Destroy;
begin
  FDatabase.Free;
  inherited;
end;

function TDatabase.Get_CharacterSet: WideString;
begin
  result := FDatabase.Params.Values['lc_ctype'];
end;

function TDatabase.Get_Connected: WordBool;
begin
  result := FDatabase.Connected;
end;

function TDatabase.Get_DatabaseName: WideString;
begin
  result := FDatabase.DatabaseName;
end;

function TDatabase.Get_PassWord: WideString;
begin
  result := FDatabase.PassWord;
end;

function TDatabase.Get_SQLDialect: Integer;
begin
  result := FDatabase.SQLDialect;
end;

function TDatabase.Get_UserName: WideString;
begin
  result := FDatabase.UserName;
end;

procedure TDatabase.Initialize;
begin
  inherited;
  FDatabase := TJvUIBDataBase.Create(nil);
end;

procedure TDatabase.Set_CharacterSet(const Value: WideString);
begin
  FDatabase.Params.Values['lc_ctype'] := Value;
end;

procedure TDatabase.Set_Connected(Value: WordBool);
begin
  FDatabase.Connected := Value;
end;

procedure TDatabase.Set_DatabaseName(const Value: WideString);
begin
  FDatabase.DatabaseName := Value;
end;

procedure TDatabase.Set_PassWord(const Value: WideString);
begin
  FDatabase.PassWord := Value;
end;

procedure TDatabase.Set_SQLDialect(Value: Integer);
begin
  FDatabase.SQLDialect := Value;
end;

procedure TDatabase.Set_UserName(const Value: WideString);
begin
  FDatabase.UserName := Value;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TDatabase, Class_Database,
    ciMultiInstance, tmApartment);
end.
