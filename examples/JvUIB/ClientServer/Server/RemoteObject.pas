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

unit RemoteObject;

interface
uses JvUIBSrv, JvUIBObj, RemoteObject_UIB, Classes, JvUIB, JvUIBConst, IdTCPServer;

type
  TRemoteObject = class(TStubRemoteObject)
  private
    FTransaction: TJvUIBTransaction;
  public
    constructor Create(Connection: TIdTCPServerConnection); override;
    destructor Destroy; override;
    { IRemoteInterface }
    function OpenQuery(SQL: String; Stream: TStream): HRESULT; override;
    function GetEmployeeCount(out count: Integer): HRESULT; override;
  end;

implementation

uses main, SysUtils;

{ TRemoteObject }

constructor TRemoteObject.Create(Connection: TIdTCPServerConnection);
begin
  inherited;
  FTransaction := TJvUIBTransaction.Create(nil);
  FTransaction.DataBase := MainForm.DataBase;
end;

destructor TRemoteObject.Destroy;
begin
  FTransaction.Free;
  inherited;
end;

function TRemoteObject.GetEmployeeCount(out count: Integer): HRESULT;
var Query: TJvUIBQuery;
begin
  try
    Query := TJvUIBQuery.Create(FTransaction);
    try
      Query.SQL.Text := 'Select Count(EMP_NO) from employee';
      Query.Open;
      count := Query.Fields.AsInteger[0];
      Result := S_OK;
    finally
      Query.Free;
    end;
  except
    Result := S_FALSE;
  end;
end;

function TRemoteObject.OpenQuery(SQL: String; Stream: TStream): HRESULT;
var Query: TJvUIBQuery;
begin
  try
    Query := TJvUIBQuery.Create(FTransaction);
    try
      Query.FetchBlobs := True;
      Query.SQL.Text := SQL;
      Query.Open;
      Query.FetchAll;
      Query.Fields.SaveToStream(Stream);
      Result := S_OK;
    finally
      Query.Free;
    end;
  except
    Result := S_FALSE;
  end;
end;

initialization
  TJvUIBObjectFactory.Create(JvUIBServer, TRemoteObject, CLSID_RemoteObject);

finalization


end.
