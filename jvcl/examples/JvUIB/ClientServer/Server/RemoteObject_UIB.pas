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

unit RemoteObject_UIB;

interface
uses JvUIBObj, JvUIBLib, Classes;

const
  CLSID_RemoteObject : TGUID = '{29A8C2BF-5DDB-494A-B923-2CD8103C3EEF}';
  IID_RemoteObject : TGUID = '{4A165F8E-3C14-4CD1-B2B9-3C2291BBC7EB}';

type
  IRemoteObject = interface(IUnKnown)
  ['{4A165F8E-3C14-4CD1-B2B9-3C2291BBC7EB}']
    function OpenQuery(SQL: String; Stream: TStream): HRESULT; stdcall; // Method 0
    function GetEmployeeCount(out count: Integer): HRESULT; stdcall;    // Method 1
  end;

  TProxyRemoteObject = class(TJvUIBProxy, IRemoteObject)
  public
    { IRemoteObject }
    function OpenQuery(SQL: String; Stream: TStream): HRESULT; stdcall;
    function GetEmployeeCount(out count: Integer): HRESULT; stdcall;

    constructor Create; override;
  end;

  TStubRemoteObject = class(TJvUIBStub, IRemoteObject)
  protected
    procedure Stub_OpenQuery;
    procedure Stub_GetEmployeeCount;
  public
    procedure Invoke(MethodID: Integer); override;
    function OpenQuery(SQL: String; Stream: TStream): HRESULT; virtual; stdcall; abstract;
    function GetEmployeeCount(out count: Integer): HRESULT; virtual; stdcall; abstract;
  end;

implementation

{ TProxyRemoteObject }

constructor TProxyRemoteObject.Create;
begin
  inherited;
  ClassID := CLSID_RemoteObject;
end;

function TProxyRemoteObject.GetEmployeeCount(out count: Integer): HRESULT;
begin
  BeginWrite;
  InitCallMethod(1);
  EndWrite;
  Count := ReadInteger;
  Result := ReadInteger;
end;

function TProxyRemoteObject.OpenQuery(SQL: String; Stream: TStream): HRESULT;
begin
  BeginWrite;
  InitCallMethod(0);
  WriteString(SQL);
  EndWrite;
  ReadStream(Stream, True);
  Stream.Seek(0, soFromBeginning);
  Result := ReadInteger;
end;

{ TStubRemoteObject }

procedure TStubRemoteObject.Invoke(MethodID: Integer);
begin
  case MethodID of
    0: Stub_OpenQuery;
    1: Stub_GetEmployeeCount;
  else
    // error !!!!
  end;
end;

procedure TStubRemoteObject.Stub_GetEmployeeCount;
var
  Count : Integer;
  Result: HRESULT;
begin
  Result := 0;
  Count := 0;
  try
    Result := GetEmployeeCount(Count);
  finally
    BeginWrite;
    WriteInteger(Count);
    WriteInteger(Result);
    EndWrite;
  end;
end;

procedure TStubRemoteObject.Stub_OpenQuery;
var
  SQL: String;
  Stream: TMemoryStream;
  Result: HResult;
begin
  SQL := ReadString;
  Stream := TMemoryStream.Create;
  Result := 0;
  try
    Result := OpenQuery(SQL, Stream);
  finally // on except output buffer must be write !!
    BeginWrite;
    WriteStream(Stream, True);
    WriteInteger(Result);
    EndWrite;
    Stream.Free;
  end;
end;

end.
