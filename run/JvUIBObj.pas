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
{ ORB Squeletons                                                               }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: Mar 16, 2003                                                  }
{                                                                              }
{******************************************************************************}
{$I JCL.INC}
{$I JvUIB.inc}
unit JvUIBObj;

interface
uses
  IdTCPClient, IdTCPServer, IdTCPConnection, IdException, SysUtils,
  JvUIBConst, JvUIBLib, Classes;

type

  EJvUIBException = class(Exception) end;

  TJvUIBConnection = class(TInterfacedObject)
  protected
    function GetConnection: TIdTCPConnection; virtual; abstract;
    procedure BeginWrite;
    procedure EndWrite;
  public
    {Write Simple Types}
    procedure WriteInteger(Value: Integer);
    procedure WriteByte(Value: Byte);
    procedure WriteWord(Value: Word);
    procedure WriteGUID(Value: TGUID);
    procedure WriteCardinal(Value: Cardinal);
    procedure WriteShortint(Value: Shortint);
    procedure WriteSmallint(Value: Smallint);
    procedure WriteLongint(Value: Longint);
    procedure WriteInt64(Value: Int64);
    procedure WriteLongword(Value: Longword);
    procedure WriteDouble(Value: Double);
    procedure WriteReal48(Value: Real48);
    procedure WriteSingle(Value: Single);
    procedure WriteExtended(Value: Extended);
    procedure WriteComp(Value: Comp);
    procedure WriteCurrency(Value: Currency);
    procedure WriteBoolean(Value: Boolean);
    procedure WriteByteBool(Value: ByteBool);
    procedure WriteWordBool(Value: WordBool);
    procedure WriteLongBool(Value: LongBool);

    {Write String}
    procedure WriteString(Value: String);
    procedure WriteWideString(Value: WideString);
    procedure WritePChar(Value: PChar);
    procedure WritePWideChar(Value: PWideChar);

    {Write Variants}
    procedure WriteVariant(var Value: Variant; compress: Boolean = false);
    procedure WriteOleVariant(var Value: OleVariant; compress: Boolean = false);
    procedure WritePVarArray(Value: PVarArray; compress: Boolean = false);

    {Write Other}
    procedure WriteStream(Stream: TStream; compress: Boolean = false);

    {Read Simple Types}
    function ReadInteger: Integer;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadGUID: TGUID;
    function ReadCardinal: Cardinal;
    function ReadShortint: Shortint;
    function ReadSmallint: Smallint;
    function ReadLongint: Longint;
    function ReadInt64: Int64;
    function ReadLongword: Longword;
    function ReadDouble: Double;
    function ReadReal48: Real48;
    function ReadSingle: Single;
    function ReadExtended: Extended;
    function ReadComp: Comp;
    function ReadCurrency: Currency;
    function ReadBoolean: Boolean;
    function ReadByteBool: ByteBool;
    function ReadWordBool: WordBool;
    function ReadLongBool: LongBool;

    {Read String}
    function ReadString: String;
    function ReadWideString: WideString;
    function ReadPChar: PChar;
    function ReadPWideChar: PWideChar;

    {Read Variants}
    function ReadVariant(DeCompress: Boolean = false): Variant;
    function ReadOleVariant(DeCompress: Boolean = false): OleVariant;
    function ReadPVarArray(DeCompress: Boolean = false): PVarArray;

    {Read Other}
    procedure ReadStream(Stream: TStream; DeCompress: Boolean = false);

    property Connection: TIdTCPConnection read GetConnection;
  end;

  TJvUIBProxy = class(TJvUIBConnection)
  private
    FTCPClient: TIdTCPClient;
    FClassID: TGUID;
    procedure SetHost(const Value: string);
    function GetHost: string;
    procedure SetPort(const Value: integer);
    function GetPort: Integer;
    function GetActive: boolean;
    procedure SetClassID(const Value: TGUID);
    procedure SetActive(const Value: boolean);
    procedure WriteCommand(Command: TServerCommand);
  protected
    function GetConnection: TIdTCPConnection; override;
    procedure InitCallMethod(Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Host: string  read GetHost write SetHost;
    property Port: integer read GetPort write SetPort;
    property Active: boolean read GetActive write SetActive;
    property ClassID: TGUID read FClassID write SetClassID;
  end;

  TJvUIBStub = class(TJvUIBConnection)
  private
    FConnection: TIdTCPServerConnection;
  protected
    function GetConnection: TIdTCPConnection; override;
  public
    procedure Invoke(MethodID: Integer); virtual;
    constructor Create(Connection: TIdTCPServerConnection); virtual;
  end;

  TJvUIBStubClass = class of TJvUIBStub;

implementation
uses
{$IFDEF DELPHI6_UP}
  VarUtils, Variants,
{$ENDIF}
  ZLib;

function SafeArrayElementTotal(VarArray: PVarArray): Integer;
var
  LDim: Integer;
begin
  Result := 1;
  for LDim := 0 to VarArray^.DimCount - 1 do
    Result := Result * VarArray^.Bounds[LDim].ElementCount;
end;

{$IFNDEF DELPHI6_UP}
const
  oleaut = 'oleaut32.dll';

function SafeArrayAllocDescriptor(DimCount: Integer; out VarArray: PVarArray): HRESULT;
  stdcall; external oleaut name 'SafeArrayAllocDescriptor';
function SafeArrayAllocData(VarArray: PVarArray): HRESULT; stdcall;
  external oleaut name 'SafeArrayAllocData';
function SafeArrayAccessData(VarArray: PVarArray; out Data: Pointer): HRESULT; stdcall;
  external oleaut name 'SafeArrayAccessData';
function SafeArrayUnaccessData(VarArray: PVarArray): HRESULT; stdcall;
  external oleaut name 'SafeArrayUnaccessData';
{$ENDIF}

{ TJvUIBClient }

constructor TJvUIBProxy.Create;
begin
  FTCPClient := TIdTCPClient.Create(nil);
end;

destructor TJvUIBProxy.Destroy;
begin
  FTCPClient.Free;
  inherited;
end;

procedure TJvUIBProxy.WriteCommand(Command: TServerCommand);
begin
  Connection.WriteBuffer(Command, SizeOf(TServerCommand));
end;

function TJvUIBProxy.GetActive: boolean;
begin
  Result := FTCPClient.Connected;
end;

function TJvUIBProxy.GetConnection: TIdTCPConnection;
begin
  result := FTCPClient;
end;

function TJvUIBProxy.GetHost: string;
begin
  result := FTCPClient.Host;
end;

function TJvUIBProxy.GetPort: Integer;
begin
  Result := FTCPClient.Port;
end;

procedure TJvUIBProxy.InitCallMethod(Value: Integer);
begin
  WriteCommand(scInvokeMethod);
  WriteInteger(Value);
end;

procedure TJvUIBProxy.SetActive(const Value: boolean);
var
  AResult: HResult;
begin
  case Value of
    True :
      with FTCPClient do
      if not Connection.Connected then
      begin
        try
          Connect;
          OpenWriteBuffer;
          WriteCommand(scGetClassObject);
          WriteGUID(FClassID);
          CloseWriteBuffer;
          AResult := ReadInteger;
          if AResult <> S_OK then
            begin
              Disconnect;
              raise EJvUIBException.Create(EJvUIB_ClassNotFound);
            end;
        except
          on E: EidSocketError do
            raise EJvUIBException.Create(EJvUIB_CantConnect);
        end;
      end;
    False:
      if FTCPClient.Connected then
        FTCPClient.Disconnect;
  end;
end;

procedure TJvUIBProxy.SetClassID(const Value: TGUID);
begin
  FClassID := Value;
end;

procedure TJvUIBProxy.SetHost(const Value: string);
begin
  FTCPClient.Host := Value;
end;

procedure TJvUIBProxy.SetPort(const Value: integer);
begin
  FTCPClient.Port := Value;
end;

{ TJvUIBObject }

constructor TJvUIBStub.Create(Connection: TIdTCPServerConnection);
begin
  FConnection := Connection;
end;

function TJvUIBStub.GetConnection: TIdTCPConnection;
begin
  Result := FConnection;
end;

procedure TJvUIBStub.Invoke(MethodID: Integer);
begin

end;

{ TJvUIBConnection }

procedure TJvUIBConnection.BeginWrite;
begin
  Connection.OpenWriteBuffer;
end;

procedure TJvUIBConnection.EndWrite;
begin
  Connection.CloseWriteBuffer;
end;

function TJvUIBConnection.ReadBoolean: Boolean;
begin
  Connection.ReadBuffer(Result, 1);
end;

function TJvUIBConnection.ReadByte: Byte;
begin
  Connection.ReadBuffer(Result, 1);
end;

function TJvUIBConnection.ReadByteBool: ByteBool;
begin
  Connection.ReadBuffer(Result, 1);
end;

function TJvUIBConnection.ReadCardinal: Cardinal;
begin
  Connection.ReadBuffer(Result, 4);
end;

function TJvUIBConnection.ReadComp: Comp;
begin
  Connection.ReadBuffer(Result, 8);
end;

function TJvUIBConnection.ReadCurrency: Currency;
begin
  Connection.ReadBuffer(Result, 8);
end;

function TJvUIBConnection.ReadDouble: Double;
begin
  Connection.ReadBuffer(Result, 8);
end;

function TJvUIBConnection.ReadExtended: Extended;
begin
  Connection.ReadBuffer(Result, 10);
end;

function TJvUIBConnection.ReadGUID: TGUID;
begin
  Connection.ReadBuffer(Result, 16);
end;

function TJvUIBConnection.ReadInt64: Int64;
begin
  Connection.ReadBuffer(Result, 8);
end;

function TJvUIBConnection.ReadInteger: Integer;
begin
  Connection.ReadBuffer(Result, 4);
end;

function TJvUIBConnection.ReadLongBool: LongBool;
begin
  Connection.ReadBuffer(Result, 4);
end;

function TJvUIBConnection.ReadLongint: Longint;
begin
  Connection.ReadBuffer(Result, 4);
end;

function TJvUIBConnection.ReadLongword: Longword;
begin
  Connection.ReadBuffer(Result, 4);
end;

function TJvUIBConnection.ReadOleVariant(DeCompress: Boolean): OleVariant;
begin
  Result := ReadVariant(DeCompress);
end;

function TJvUIBConnection.ReadPChar: PChar;
var
  StrCount: integer;
begin
  Connection.ReadBuffer(StrCount, 4);
  if (StrCount > 0) then
  begin
    GetMem(Result, StrCount);
    Connection.ReadBuffer(Result^, StrCount);
  end else result := nil;
end;

function TJvUIBConnection.ReadPVarArray(DeCompress: Boolean): PVarArray;
var
  DataPtr: Pointer;
  DimCount, Flags: word;
  ElementSize: integer;
  InBytes: Integer;
  InBuf: Pointer;
 begin
  Connection.ReadBuffer(DimCount, 2);
  Connection.ReadBuffer(Flags, 2);
  Connection.ReadBuffer(ElementSize, 4);
  SafeArrayAllocDescriptor(DimCount, Result);
  Result.Flags    := Flags;
  Result.ElementSize := ElementSize;
  Connection.ReadBuffer(Result.Bounds, 8*DimCount);
  SafeArrayAllocData(Result);
  case DeCompress of
  True:
    begin
      Connection.ReadBuffer(InBytes, SizeOf(InBytes));
      if InBytes > 0 then
      begin
        SafeArrayAccessData(Result, DataPtr);
        GetMem(InBuf, InBytes);
        try
          Connection.ReadBuffer(InBuf^, InBytes);
          DecompressToUserBuf(InBuf, InBytes, DataPtr, SafeArrayElementTotal(Result) * ElementSize);
        finally
          FreeMem(InBuf, InBytes);
          SafeArrayUnaccessData(Result);
        end;
      end;
    end;
  False:
    begin
      SafeArrayAccessData(Result, DataPtr);
      try
        Connection.ReadBuffer(DataPtr^, SafeArrayElementTotal(Result) * ElementSize);
      finally
        SafeArrayUnaccessData(Result);
      end;
    end;
  end;
end;

function TJvUIBConnection.ReadPWideChar: PWideChar;
var
  StrCount: integer;
begin
  Connection.ReadBuffer(StrCount, 4);
  if (StrCount > 0) then
  begin
    GetMem(Result, StrCount);
    Connection.ReadBuffer(Result^, StrCount);
  end else result := nil;
end;

function TJvUIBConnection.ReadReal48: Real48;
begin
  Connection.ReadBuffer(Result, 6);
end;

function TJvUIBConnection.ReadShortint: Shortint;
begin
  Connection.ReadBuffer(Result, 1);
end;

function TJvUIBConnection.ReadSingle: Single;
begin
  Connection.ReadBuffer(Result, 4);
end;

function TJvUIBConnection.ReadSmallint: Smallint;
begin
  Connection.ReadBuffer(Result, 2);
end;

procedure TJvUIBConnection.ReadStream(Stream: TStream; DeCompress: Boolean = false);
var
  InPutBuffer, OutPutBuffer: Pointer;
  InPutSize, OutPutSize: Integer;
begin
  if DeCompress then
  begin
    InPutSize  := ReadInteger;
    OutPutSize := ReadInteger;
    getmem(InPutBuffer, InPutSize);
    GetMem(OutPutBuffer, OutPutSize);
    Connection.ReadBuffer(InPutBuffer^, InPutSize);
    DecompressToUserBuf(InPutBuffer, InPutSize, OutPutBuffer, OutPutSize);
    FreeMem(InPutBuffer, InPutSize);
    Stream.Seek(0, soFromBeginning);
    Stream.Write(OutPutBuffer^, OutPutSize);
    FreeMem(OutPutBuffer, OutPutSize);
  end else
    Connection.ReadStream(Stream, ReadInteger);
end;

function TJvUIBConnection.ReadString: String;
var
  StrCount: integer;
begin
  Connection.ReadBuffer(StrCount, 4);
  if (StrCount > 0) then
  begin
    SetLength(Result, StrCount);
    Connection.ReadBuffer(Result[1], StrCount);
  end;
end;

function TJvUIBConnection.ReadVariant(DeCompress: Boolean): Variant;
begin
  VarClear(Result);
  Connection.ReadBuffer(TVarData(Result).VType, 2);
  case TVarData(Result).VType of
    varEmpty: {Nothing to do};
    varNull: {Nothing to do};
    varSmallInt: Connection.ReadBuffer(TVarData(Result).VSmallInt, 2);
    varInteger:  Connection.ReadBuffer(TVarData(Result).VInteger, 4);
    varSingle:   Connection.ReadBuffer(TVarData(Result).VSingle, 4);
    varDouble:   Connection.ReadBuffer(TVarData(Result).VDouble, 8);
    varCurrency: Connection.ReadBuffer(TVarData(Result).VCurrency, 8);
    varDate:     Connection.ReadBuffer(TVarData(Result).VDate, 8);
    varOleStr:   TVarData(Result).VOleStr := ReadPWideChar;
    varBoolean:  Connection.ReadBuffer(TVarData(Result).VBoolean, 2);
{$IFDEF DELPHI6_UP}
    varShortInt: Connection.ReadBuffer(TVarData(Result).VShortInt, 1);
    varWord:     Connection.ReadBuffer(TVarData(Result).VWord, 2);
    varLongWord: Connection.ReadBuffer(TVarData(Result).VLongWord, 4);
    varInt64:    Connection.ReadBuffer(TVarData(Result).VInt64, 8);
{$ENDIF}
    varByte:     Connection.ReadBuffer(TVarData(Result).VByte, 1);
    varString:   TVarData(Result).VString := ReadPChar;
  else
      if TVarData(Result).VType and varArray = varArray then
     TVarData(Result).VArray := ReadPVarArray(DeCompress) else
    raise EJvUIBException.Create(EJvUIB_DataType);
  end
end;

function TJvUIBConnection.ReadWideString: WideString;
var
  StrCount: integer;
begin
  Connection.ReadBuffer(StrCount, 4);
  if (StrCount > 0) then
  begin
    SetLength(Result, StrCount);
    Connection.ReadBuffer(Result[1], StrCount*2);
  end;
end;

function TJvUIBConnection.ReadWord: Word;
begin
  Connection.ReadBuffer(Result, 2);
end;

function TJvUIBConnection.ReadWordBool: WordBool;
begin
  Connection.ReadBuffer(Result, 2);
end;

procedure TJvUIBConnection.WriteBoolean(Value: Boolean);
begin
  Connection.WriteBuffer(Value, 1);
end;

procedure TJvUIBConnection.WriteByte(Value: Byte);
begin
  Connection.WriteBuffer(Value, 1);
end;

procedure TJvUIBConnection.WriteByteBool(Value: ByteBool);
begin
  Connection.WriteBuffer(Value, 1);
end;

procedure TJvUIBConnection.WriteCardinal(Value: Cardinal);
begin
  Connection.WriteBuffer(Value, 4);
end;

procedure TJvUIBConnection.WriteComp(Value: Comp);
begin
  Connection.WriteBuffer(Value, 8);
end;

procedure TJvUIBConnection.WriteCurrency(Value: Currency);
begin
  Connection.WriteBuffer(Value, 8);
end;

procedure TJvUIBConnection.WriteDouble(Value: Double);
begin
  Connection.WriteBuffer(Value, 8);
end;

procedure TJvUIBConnection.WriteExtended(Value: Extended);
begin
  Connection.WriteBuffer(Value, 10);
end;

procedure TJvUIBConnection.WriteGUID(Value: TGUID);
begin
  Connection.WriteBuffer(Value, 16);
end;

procedure TJvUIBConnection.WriteInt64(Value: Int64);
begin
  Connection.WriteBuffer(Value, 8);
end;

procedure TJvUIBConnection.WriteInteger(Value: Integer);
begin
  Connection.WriteBuffer(Value, 4);
end;

procedure TJvUIBConnection.WriteLongBool(Value: LongBool);
begin
  Connection.WriteBuffer(Value, 4);
end;

procedure TJvUIBConnection.WriteLongint(Value: Longint);
begin
  Connection.WriteBuffer(Value, 4);
end;

procedure TJvUIBConnection.WriteLongword(Value: Longword);
begin
  Connection.WriteBuffer(Value, 4);
end;

procedure TJvUIBConnection.WriteOleVariant(var Value: OleVariant; compress: Boolean);
begin
  WriteVariant(Variant(Value), Compress);
end;

procedure TJvUIBConnection.WritePChar(Value: PChar);
var StrCount: integer;
begin
  StrCount := Length(Value);
  Connection.WriteBuffer(StrCount, 4);
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount);
end;

procedure TJvUIBConnection.WritePVarArray(Value: PVarArray; compress: Boolean);
var
  DataPtr: Pointer;
  OutBuf: Pointer;
  OutBytes: Integer;
begin
  OutBytes := 0;
  OutBuf   := nil;
  SafeArrayAccessData(Value, DataPtr);
  try
    Connection.WriteBuffer(Value^, 8); // DimCount + Flags + ElementSize
    Connection.WriteBuffer(Value^.Bounds, 8*Value.DimCount); // Bounds
    case compress of
      True:
        begin
          CompressBuf(DataPtr, SafeArrayElementTotal(Value) * Value.ElementSize, OutBuf, OutBytes);
          Connection.WriteBuffer(OutBytes, SizeOf(OutBytes));
          if OutBytes > 0 then
          begin
            Connection.WriteBuffer(OutBuf^, OutBytes);
            FreeMem(OutBuf, OutBytes);
          end;
        end;
      False:
        begin
          Connection.WriteBuffer(DataPtr^, SafeArrayElementTotal(Value) * Value.ElementSize);
        end;
    end;
  finally
    SafeArrayUnaccessData(Value);
  end;
end;

procedure TJvUIBConnection.WritePWideChar(Value: PWideChar);
var StrCount: integer;
begin
  StrCount := Length(Value)*2 + 2;
  Connection.WriteBuffer(StrCount, 4);
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount);
end;

procedure TJvUIBConnection.WriteReal48(Value: Real48);
begin
  Connection.WriteBuffer(Value, 6);
end;

procedure TJvUIBConnection.WriteShortint(Value: Shortint);
begin
  Connection.WriteBuffer(Value, 1);
end;

procedure TJvUIBConnection.WriteSingle(Value: Single);
begin
  Connection.WriteBuffer(Value, 4);
end;

procedure TJvUIBConnection.WriteSmallint(Value: Smallint);
begin
  Connection.WriteBuffer(Value, 2);
end;

procedure TJvUIBConnection.WriteStream(Stream: TStream; compress: Boolean = false);
var
  InPutBuffer, OutPutBuffer: Pointer;
  InPutSize, OutPutSize: Integer;
begin
  if Compress then
  begin
    InPutSize := Stream.Size;
    GetMem(InPutBuffer, InPutSize);
    Stream.Seek(0, soFromBeginning);
    Stream.Read(InPutBuffer^, InPutSize);
    CompressBuf(InPutBuffer, InPutSize, OutPutBuffer, OutPutSize);
    FreeMem(InPutBuffer, InPutSize);
    WriteInteger(OutPutSize);
    WriteInteger(InPutSize);
    Connection.WriteBuffer(OutPutBuffer^, OutPutSize);
    freemem(OutPutBuffer, OutPutSize);
  end else
  begin
    WriteInteger(Stream.Size);
    Connection.WriteStream(Stream);
  end;
end;

procedure TJvUIBConnection.WriteString(Value: String);
var StrCount: integer;
begin
  StrCount := Length(Value);
  Connection.WriteBuffer(StrCount, 4);
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount);
end;

procedure TJvUIBConnection.WriteVariant(var Value: Variant;
  compress: Boolean);
begin
  Connection.WriteBuffer(TVarData(Value).VType, 2);
  case TVarData(Value).VType of
    varEmpty: {Nothing to do};
    varNull: {Nothing to do};
    varSmallInt: Connection.WriteBuffer(TVarData(Value).VSmallInt, 2);
    varInteger:  Connection.WriteBuffer(TVarData(Value).VInteger, 4);
    varSingle:   Connection.WriteBuffer(TVarData(Value).VSingle, 4);
    varDouble:   Connection.WriteBuffer(TVarData(Value).VDouble, 8);
    varCurrency: Connection.WriteBuffer(TVarData(Value).VCurrency, 8);
    varDate:     Connection.WriteBuffer(TVarData(Value).VDate, 8);
    varOleStr:   WritePWideChar(TVarData(Value).VOleStr);
    varBoolean:  Connection.WriteBuffer(TVarData(Value).VBoolean, 2);
{$IFDEF DELPHI6_UP}
    varShortInt: Connection.WriteBuffer(TVarData(Value).VShortInt, 1);
    varWord:     Connection.WriteBuffer(TVarData(Value).VWord, 2);
    varLongWord: Connection.WriteBuffer(TVarData(Value).VLongWord, 4);
    varInt64:    Connection.WriteBuffer(TVarData(Value).VInt64, 8);
{$ENDIF}
    varByte:     Connection.WriteBuffer(TVarData(Value).VByte, 1);
    varString:   WritePChar(TVarData(Value).VString);
  else
    if TVarData(Value).VType and varArray = varArray then
       WritePVarArray(TVarData(Value).VArray, compress) else
       raise EJvUIBException.Create(EJvUIB_DataType);
  end;
end;

procedure TJvUIBConnection.WriteWideString(Value: WideString);
var StrCount: integer;
begin
  StrCount := Length(Value);
  Connection.WriteBuffer(StrCount, 4);
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount*2);
end;

procedure TJvUIBConnection.WriteWord(Value: Word);
begin
  Connection.WriteBuffer(Value, 2);
end;

procedure TJvUIBConnection.WriteWordBool(Value: WordBool);
begin
  Connection.WriteBuffer(Value, 2);
end;

end.
