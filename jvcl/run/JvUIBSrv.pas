{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JvUIBLib.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ ORB Server                                                                   }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: Mar 16, 2003                                                  }
{                                                                              }
{******************************************************************************}
{$I JCL.INC}
{$I JvUIB.inc}
unit JvUIBSrv;

interface
uses Classes, IdTCPServer, SysUtils, JvUIBObj, JvUIBConst;

type

  TJvUIBPeerThread = class;
  TJvUIBObjectFactory = class;

  TJvUIBServer = class
  private
    FTCPServer: TidTCPServer;
    FFactoryList: TJvUIBObjectFactory;
    FLock: TMultiReadExclusiveWriteSynchronizer;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetDefaultPort: integer;
    procedure SetDefaultPort(const Value: integer);

    procedure AddObjectFactory(Factory: TJvUIBObjectFactory);
    procedure RemoveObjectFactory(Factory: TJvUIBObjectFactory);
    procedure FreeFactories;
    function GetFactoryFromClassID(const ClassID: TGUID): TJvUIBObjectFactory;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property DefaultPort: integer read GetDefaultPort write SetDefaultPort;
  end;

  TJvUIBObjectFactory = class
  private
    FNext: TJvUIBObjectFactory;
    FClassID: TGUID;
    FNetClass: TJvUIBStubClass;
   public
    constructor Create(Server: TJvUIBServer; NetClass: TJvUIBStubClass;
      const ClassID: TGUID);
    function CreateInstance(Connection: TIdTCPServerConnection): TJvUIBStub;
    destructor Destroy; override;
    property ClassID: TGUID read FClassID;
    property JvUIBClass: TJvUIBStubClass read FNetClass;
  end;

  TJvUIBPeerThread = class(TIdPeerThread)
  protected
    procedure Run; override;
    procedure CleanUp; override;
    procedure GetClassObject;
    procedure InvokeMethod;
  end;

var
  JvUIBServer: TJvUIBServer;

implementation

uses IdThread, IdException, IdStackConsts, IdTCPConnection;

{ TJvUIBServer }

constructor TJvUIBServer.Create;
begin
  inherited;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FTCPServer := TidTCPServer.Create(nil);
  FTCPServer.ThreadClass  := TJvUIBPeerThread;
end;

destructor TJvUIBServer.Destroy;
begin
  FTCPServer.Free;
  FreeFactories;
  FLock.Free;
  inherited Destroy;
end;

function TJvUIBServer.GetActive: Boolean;
begin
  result := FTCPServer.Active;
end;

function TJvUIBServer.GetDefaultPort: integer;
begin
  result := FTCPServer.DefaultPort;
end;

procedure TJvUIBServer.SetActive(const Value: Boolean);
begin
  FTCPServer.Active := Value;
end;

procedure TJvUIBServer.SetDefaultPort(const Value: integer);
begin
  FTCPServer.DefaultPort := Value;
end;

procedure TJvUIBServer.Start;
begin
  Active := True;
end;

procedure TJvUIBServer.Stop;
begin
  Active := False;
end;

procedure TJvUIBServer.AddObjectFactory(
  Factory: TJvUIBObjectFactory);
begin
  FLock.BeginWrite;
  try
    Factory.FNext := FFactoryList;
    FFactoryList := Factory;
  finally
    FLock.EndWrite;
  end;
end;

procedure TJvUIBServer.FreeFactories;
var
  Factory, Next: TJvUIBObjectFactory;
begin
  FLock.BeginWrite;
  try
    Factory := FFactoryList;
    while Factory <> nil do
    begin
      Next := Factory.FNext;
      Factory.Free;
      Factory := Next;
    end;
  finally
    FLock.EndWrite;
  end;
end;

function TJvUIBServer.GetFactoryFromClassID(
  const ClassID: TGUID): TJvUIBObjectFactory;
begin
  FLock.BeginRead;
  try
    Result := FFactoryList;
    while Result <> nil do
    begin
{$IFDEF DELPHI_UP}
      if IsEqualGUID(Result.ClassID, ClassID) then Exit;
{$ELSE}
      if CompareMem(@Result.ClassID, @ClassID, SizeOf(TGUID)) then Exit;
{$ENDIF}
      Result := Result.FNext;
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TJvUIBServer.RemoveObjectFactory(
  Factory: TJvUIBObjectFactory);
var
  F, P: TJvUIBObjectFactory;
begin
  FLock.BeginWrite;
  try
    P := nil;
    F := FFactoryList;
    while F <> nil do
    begin
      if F = Factory then
      begin
        if P <> nil then
          P.FNext := F.FNext else
          FFactoryList := F.FNext;
        Exit;
      end;
      P := F;
      F := F.FNext;
    end;
  finally
    FLock.EndWrite;
  end;
end;

{ TJvUIBObjectFactory }

constructor TJvUIBObjectFactory.Create(Server: TJvUIBServer;
  NetClass: TJvUIBStubClass; const ClassID: TGUID);
begin
  Assert(Assigned(Server));
  FNetClass := NetClass;
  FClassID := ClassID;
  JvUIBServer.AddObjectFactory(Self);
end;

function TJvUIBObjectFactory.CreateInstance(Connection: TIdTCPServerConnection): TJvUIBStub;
begin
  Result := FNetClass.Create(Connection);
  IUnknown(Result)._AddRef;
end;

destructor TJvUIBObjectFactory.Destroy;
begin
  JvUIBServer.RemoveObjectFactory(Self);
  inherited;
end;

{ TJvUIBPeerThread }

procedure TJvUIBPeerThread.CleanUp;
begin
  if (FData <> nil) then
  begin
    IUnKnown(TJvUIBStub(FData))._Release;
    FData := nil;
  end;
  inherited CleanUp;
end;

procedure TJvUIBPeerThread.GetClassObject;
var
  GUID: TGUID;
  Factory: TJvUIBObjectFactory;
  Result: HResult;
begin
  Result := S_OK;
  with FConnection do
  begin
    ReadBuffer(GUID, 16);
    Factory := JvUIBServer.GetFactoryFromClassID(GUID);
    if Factory <> nil then
    begin
      if (FData <> nil) then
      begin
        IUnKnown(TJvUIBStub(FData))._Release;
        FData := nil;
      end;
      FData := Factory.CreateInstance(FConnection);
      OpenWriteBuffer;
      Connection.WriteBuffer(Result, 4);
      CloseWriteBuffer;
    end else
    begin
      Result := S_FALSE;
      OpenWriteBuffer;
      Connection.WriteBuffer(Result, 4);
      CloseWriteBuffer;
    end;
  end;
end;

procedure TJvUIBPeerThread.InvokeMethod;
var MethodID: Integer;
begin
  if FData <> nil then
  begin
    FConnection.ReadBuffer(MethodID, 4);
    TJvUIBStub(FData).Invoke(MethodID);
  end else
  begin
    // todo Disconnect !!!
  end;
end;

procedure TJvUIBPeerThread.Run;
var
  Command: TServerCommand;
begin
  try
    while Connection.Connected do
    begin
      FConnection.ReadBuffer(Command, SizeOf(TServerCommand));
      case Command of
        scGetClassObject: GetClassObject;
        scInvokeMethod  : InvokeMethod;
      else
        // Todo Deconnecter
      end;
    end;
  except
    on E: EIdSocketError do begin
      case E.LastError of
        Id_WSAECONNABORTED
         , Id_WSAECONNRESET:
          Connection.Disconnect;
      end;
    end;
  end;
  if not Connection.Connected then begin
    Stop;
  end;
end;


initialization
  JvUIBServer := TJvUIBServer.Create;

finalization
  JvUIBServer.Free;

end.
