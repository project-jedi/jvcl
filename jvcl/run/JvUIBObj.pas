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
{ ORB Skeletons                                                                }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: Mar 16, 2003                                                  }
{                                                                              }
{******************************************************************************}

{$I jvcl.inc}
{$I jvuib.inc}

unit JvUIBObj;

interface

uses
  IdTCPClient, IdTCPServer, IdTCPConnection, IdException, SysUtils,
  JvUIBConst, JvUIBLib, Classes;

type
{$IFNDEF COMPILER7_UP}
  TDate = TDateTime;
{$ENDIF COMPILER6_UP}
{$IFDEF VisualCLX}
  TDate = TDateTime;
{$ENDIF VisualCLX}
  EJvUIBException = class(Exception);

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
    procedure WriteString(Value: string);
    procedure WriteWideString(Value: WideString);
    procedure WritePChar(Value: PChar);
    procedure WritePWideChar(Value: PWideChar);

    {Write Variants}
    procedure WriteVariant(var Value: Variant; Compress: Boolean = False);
    procedure WriteOleVariant(var Value: OleVariant; Compress: Boolean = False);
    procedure WritePVarArray(Value: PVarArray; Compress: Boolean = False);

    {Write Other}
    procedure WriteStream(Stream: TStream; Compress: Boolean = False);

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
    function ReadString: string;
    function ReadWideString: WideString;
    function ReadPChar: PChar;
    function ReadPWideChar: PWideChar;

    {Read Variants}
    function ReadVariant(DeCompress: Boolean = False): Variant;
    function ReadOleVariant(DeCompress: Boolean = False): OleVariant;
    function ReadPVarArray(DeCompress: Boolean = False): PVarArray;

    {Read Other}
    procedure ReadStream(Stream: TStream; DeCompress: Boolean = False);

    property Connection: TIdTCPConnection read GetConnection;
  end;

  TJvUIBProxy = class(TJvUIBConnection)
  private
    FTCPClient: TIdTCPClient;
    FClassID: TGUID;
    procedure SetHost(const Value: string);
    function GetHost: string;
    procedure SetPort(const Value: Integer);
    function GetPort: Integer;
    function GetActive: Boolean;
    procedure SetClassID(const Value: TGUID);
    procedure SetActive(const Value: Boolean);
    procedure WriteCommand(Command: TServerCommand);
  protected
    function GetConnection: TIdTCPConnection; override;
    procedure InitCallMethod(Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
    property Active: Boolean read GetActive write SetActive;
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
  {$ENDIF DELPHI6_UP}
  ZLib;

{$IFNDEF DELPHI6_UP}
const
  OleAutDllName = 'oleaut32.dll';

function SafeArrayAllocDescriptor(DimCount: Integer; out VarArray: PVarArray): HRESULT;
  stdcall; external OleAutDllName name 'SafeArrayAllocDescriptor';

function SafeArrayAllocData(VarArray: PVarArray): HRESULT; stdcall;
  external OleAutDllName name 'SafeArrayAllocData';

function SafeArrayAccessData(VarArray: PVarArray; out Data: Pointer): HRESULT; stdcall;
  external OleAutDllName name 'SafeArrayAccessData';

function SafeArrayUnaccessData(VarArray: PVarArray): HRESULT; stdcall;
  external OleAutDllName name 'SafeArrayUnaccessData';

{$ENDIF DELPHI6_UP}

function SafeArrayElementTotal(VarArray: PVarArray): Integer;
var
  LDim: Integer;
begin
  Result := 1;
  for LDim := 0 to VarArray^.DimCount - 1 do
    Result := Result * VarArray^.Bounds[LDim].ElementCount;
end;

//=== { TJvUIBClient } =======================================================

constructor TJvUIBProxy.Create;
begin
  // (rom) added inherited Create
  inherited Create;
  FTCPClient := TIdTCPClient.Create(nil);
end;

destructor TJvUIBProxy.Destroy;
begin
  FTCPClient.Free;
  inherited Destroy;
end;

procedure TJvUIBProxy.WriteCommand(Command: TServerCommand);
begin
  Connection.WriteBuffer(Command, SizeOf(TServerCommand));
end;

function TJvUIBProxy.GetActive: Boolean;
begin
  Result := FTCPClient.Connected;
end;

function TJvUIBProxy.GetConnection: TIdTCPConnection;
begin
  Result := FTCPClient;
end;

function TJvUIBProxy.GetHost: string;
begin
  Result := FTCPClient.Host;
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

procedure TJvUIBProxy.SetActive(const Value: Boolean);
var
  Ret: HRESULT;
begin
  case Value of
    False:
      if FTCPClient.Connected then
        FTCPClient.Disconnect;
    True:
      with FTCPClient do
        if not Connection.Connected then
        begin
          try
            Connect;
            OpenWriteBuffer;
            WriteCommand(scGetClassObject);
            WriteGUID(FClassID);
            CloseWriteBuffer;
            Ret := ReadInteger;
            if Ret <> S_OK then
            begin
              Disconnect;
              raise EJvUIBException.Create(EJvUIB_ClassNotFound);
            end;
          except
            on E: EidSocketError do
              raise EJvUIBException.Create(EJvUIB_CantConnect);
          end;
        end;
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

procedure TJvUIBProxy.SetPort(const Value: Integer);
begin
  FTCPClient.Port := Value;
end;

//=== { TJvUIBObject } =======================================================

constructor TJvUIBStub.Create(Connection: TIdTCPServerConnection);
begin
  // (rom) added inherited Create
  inherited Create;
  FConnection := Connection;
end;

function TJvUIBStub.GetConnection: TIdTCPConnection;
begin
  Result := FConnection;
end;

procedure TJvUIBStub.Invoke(MethodID: Integer);
begin
end;

//=== { TJvUIBConnection } ===================================================

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
  Connection.ReadBuffer(Result, SizeOf(Boolean));
end;

function TJvUIBConnection.ReadByte: Byte;
begin
  Connection.ReadBuffer(Result, SizeOf(Byte));
end;

function TJvUIBConnection.ReadByteBool: ByteBool;
begin
  Connection.ReadBuffer(Result, SizeOf(ByteBool));
end;

function TJvUIBConnection.ReadCardinal: Cardinal;
begin
  Connection.ReadBuffer(Result, SizeOf(Cardinal));
end;

function TJvUIBConnection.ReadComp: Comp;
begin
  Connection.ReadBuffer(Result, SizeOf(Comp));
end;

function TJvUIBConnection.ReadCurrency: Currency;
begin
  Connection.ReadBuffer(Result, SizeOf(Currency));
end;

function TJvUIBConnection.ReadDouble: Double;
begin
  Connection.ReadBuffer(Result, SizeOf(Double));
end;

function TJvUIBConnection.ReadExtended: Extended;
begin
  Connection.ReadBuffer(Result, SizeOf(Extended));
end;

function TJvUIBConnection.ReadGUID: TGUID;
begin
  Connection.ReadBuffer(Result, SizeOf(TGUID));
end;

function TJvUIBConnection.ReadInt64: Int64;
begin
  Connection.ReadBuffer(Result, SizeOf(Int64));
end;

function TJvUIBConnection.ReadInteger: Integer;
begin
  Connection.ReadBuffer(Result, SizeOf(Integer));
end;

function TJvUIBConnection.ReadLongBool: LongBool;
begin
  Connection.ReadBuffer(Result, 4);
end;

function TJvUIBConnection.ReadLongint: Longint;
begin
  Connection.ReadBuffer(Result, SizeOf(Longint));
end;

function TJvUIBConnection.ReadLongword: Longword;
begin
  Connection.ReadBuffer(Result, SizeOf(Longword));
end;

function TJvUIBConnection.ReadOleVariant(DeCompress: Boolean): OleVariant;
begin
  Result := ReadVariant(DeCompress);
end;

function TJvUIBConnection.ReadPChar: PChar;
var
  StrCount: Integer;
begin
  Connection.ReadBuffer(StrCount, SizeOf(StrCount));
  if StrCount > 0 then
  begin
    GetMem(Result, StrCount);
    Connection.ReadBuffer(Result^, StrCount);
  end
  else
    Result := nil;
end;

function TJvUIBConnection.ReadPVarArray(DeCompress: Boolean): PVarArray;
var
  DataPtr: Pointer;
  DimCount, Flags: Word;
  ElementSize: Integer;
  InBytes: Integer;
  InBuf: Pointer;
begin
  Connection.ReadBuffer(DimCount, SizeOf(DimCount));
  Connection.ReadBuffer(Flags, SizeOf(Flags));
  Connection.ReadBuffer(ElementSize, SizeOf(ElementSize));
  SafeArrayAllocDescriptor(DimCount, Result);
  Result.Flags := Flags;
  Result.ElementSize := ElementSize;
  Connection.ReadBuffer(Result.Bounds, 8 * DimCount);
  SafeArrayAllocData(Result);
  case DeCompress of
    False:
      begin
        SafeArrayAccessData(Result, DataPtr);
        try
          Connection.ReadBuffer(DataPtr^, SafeArrayElementTotal(Result) * ElementSize);
        finally
          SafeArrayUnaccessData(Result);
        end;
      end;
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
  end;
end;

function TJvUIBConnection.ReadPWideChar: PWideChar;
var
  StrCount: Integer;
begin
  Connection.ReadBuffer(StrCount, SizeOf(StrCount));
  if StrCount > 0 then
  begin
    GetMem(Result, StrCount);
    Connection.ReadBuffer(Result^, StrCount);
  end
  else
    Result := nil;
end;

function TJvUIBConnection.ReadReal48: Real48;
begin
  Connection.ReadBuffer(Result, SizeOf(Real48));
end;

function TJvUIBConnection.ReadShortint: Shortint;
begin
  Connection.ReadBuffer(Result, SizeOf(Shortint));
end;

function TJvUIBConnection.ReadSingle: Single;
begin
  Connection.ReadBuffer(Result, SizeOf(Single));
end;

function TJvUIBConnection.ReadSmallint: Smallint;
begin
  Connection.ReadBuffer(Result, SizeOf(Smallint));
end;

procedure TJvUIBConnection.ReadStream(Stream: TStream; DeCompress: Boolean = False);
var
  InPutBuffer, OutPutBuffer: Pointer;
  InPutSize, OutPutSize: Integer;
begin
  if DeCompress then
  begin
    InPutSize := ReadInteger;
    OutPutSize := ReadInteger;
    getmem(InPutBuffer, InPutSize);
    GetMem(OutPutBuffer, OutPutSize);
    Connection.ReadBuffer(InPutBuffer^, InPutSize);
    DecompressToUserBuf(InPutBuffer, InPutSize, OutPutBuffer, OutPutSize);
    FreeMem(InPutBuffer, InPutSize);
    Stream.Seek(0, soFromBeginning);
    Stream.Write(OutPutBuffer^, OutPutSize);
    FreeMem(OutPutBuffer, OutPutSize);
  end
  else
    Connection.ReadStream(Stream, ReadInteger);
end;

function TJvUIBConnection.ReadString: string;
var
  StrCount: Integer;
begin
  Connection.ReadBuffer(StrCount, SizeOf(StrCount));
  if StrCount > 0 then
  begin
    SetLength(Result, StrCount);
    Connection.ReadBuffer(Result[1], StrCount);
  end;
end;

function TJvUIBConnection.ReadVariant(DeCompress: Boolean): Variant;
begin
  VarClear(Result);
  Connection.ReadBuffer(TVarData(Result).VType, 2);
  // (rom) why not call the Read* methods?
  case TVarData(Result).VType of
    varEmpty:
      {Nothing to do};
    varNull:
     {Nothing to do};
    varSmallInt:
      Connection.ReadBuffer(TVarData(Result).VSmallInt, SizeOf(Smallint));
    varInteger:
      Connection.ReadBuffer(TVarData(Result).VInteger, SizeOf(Integer));
    varSingle:
      Connection.ReadBuffer(TVarData(Result).VSingle,  SizeOf(Single));
    varDouble:
      Connection.ReadBuffer(TVarData(Result).VDouble, SizeOf(Double));
    varCurrency:
      Connection.ReadBuffer(TVarData(Result).VCurrency, SizeOf(Currency));
    varDate:
      Connection.ReadBuffer(TVarData(Result).VDate, SizeOf(TDate));
    varOleStr:
      TVarData(Result).VOleStr := ReadPWideChar;
    varBoolean:
      // (rom) Beware! Size inconsistency!
      Connection.ReadBuffer(TVarData(Result).VBoolean, 2);
    {$IFDEF DELPHI6_UP}
    varShortInt:
      Connection.ReadBuffer(TVarData(Result).VShortInt, SizeOf(Shortint));
    varWord:
      Connection.ReadBuffer(TVarData(Result).VWord, SizeOf(Word));
    varLongWord:
      Connection.ReadBuffer(TVarData(Result).VLongWord, SizeOf(Longword));
    varInt64:
      Connection.ReadBuffer(TVarData(Result).VInt64, SizeOf(Int64));
    {$ENDIF DELPHI6_UP}
    varByte:
      Connection.ReadBuffer(TVarData(Result).VByte, SizeOf(Byte));
    varString:
      TVarData(Result).VString := ReadPChar;
  else
    if TVarData(Result).VType and varArray = varArray then
      TVarData(Result).VArray := ReadPVarArray(DeCompress)
    else
      raise EJvUIBException.Create(EJvUIB_DataType);
  end
end;

function TJvUIBConnection.ReadWideString: WideString;
var
  StrCount: Integer;
begin
  Connection.ReadBuffer(StrCount, SizeOf(StrCount));
  if StrCount > 0 then
  begin
    SetLength(Result, StrCount);
    Connection.ReadBuffer(Result[1], StrCount * SizeOf(WideChar));
  end;
end;

function TJvUIBConnection.ReadWord: Word;
begin
  Connection.ReadBuffer(Result, SizeOf(Word));
end;

function TJvUIBConnection.ReadWordBool: WordBool;
begin
  Connection.ReadBuffer(Result, SizeOf(WordBool));
end;

procedure TJvUIBConnection.WriteBoolean(Value: Boolean);
begin
  Connection.WriteBuffer(Value, SizeOf(Boolean));
end;

procedure TJvUIBConnection.WriteByte(Value: Byte);
begin
  Connection.WriteBuffer(Value, SizeOf(Byte));
end;

procedure TJvUIBConnection.WriteByteBool(Value: ByteBool);
begin
  Connection.WriteBuffer(Value, SizeOf(ByteBool));
end;

procedure TJvUIBConnection.WriteCardinal(Value: Cardinal);
begin
  Connection.WriteBuffer(Value, SizeOf(Cardinal));
end;

procedure TJvUIBConnection.WriteComp(Value: Comp);
begin
  Connection.WriteBuffer(Value, SizeOf(Comp));
end;

procedure TJvUIBConnection.WriteCurrency(Value: Currency);
begin
  Connection.WriteBuffer(Value, SizeOf(Currency));
end;

procedure TJvUIBConnection.WriteDouble(Value: Double);
begin
  Connection.WriteBuffer(Value, SizeOf(Double));
end;

procedure TJvUIBConnection.WriteExtended(Value: Extended);
begin
  Connection.WriteBuffer(Value, SizeOf(Extended));
end;

procedure TJvUIBConnection.WriteGUID(Value: TGUID);
begin
  Connection.WriteBuffer(Value, SizeOf(TGUID));
end;

procedure TJvUIBConnection.WriteInt64(Value: Int64);
begin
  Connection.WriteBuffer(Value, SizeOf(Int64));
end;

procedure TJvUIBConnection.WriteInteger(Value: Integer);
begin
  Connection.WriteBuffer(Value, SizeOf(Integer));
end;

procedure TJvUIBConnection.WriteLongBool(Value: LongBool);
begin
  Connection.WriteBuffer(Value, SizeOf(LongBool));
end;

procedure TJvUIBConnection.WriteLongint(Value: Longint);
begin
  Connection.WriteBuffer(Value, SizeOf(Longint));
end;

procedure TJvUIBConnection.WriteLongword(Value: Longword);
begin
  Connection.WriteBuffer(Value, SizeOf(Longword));
end;

procedure TJvUIBConnection.WriteOleVariant(var Value: OleVariant; Compress: Boolean);
begin
  WriteVariant(Variant(Value), Compress);
end;

procedure TJvUIBConnection.WritePChar(Value: PChar);
var
  StrCount: Integer;
begin
  StrCount := Length(Value);
  Connection.WriteBuffer(StrCount, SizeOf(StrCount));
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount);
end;

procedure TJvUIBConnection.WritePVarArray(Value: PVarArray; Compress: Boolean);
var
  DataPtr: Pointer;
  OutBuf: Pointer;
  OutBytes: Integer;
begin
  OutBytes := 0;
  OutBuf := nil;
  SafeArrayAccessData(Value, DataPtr);
  try
    Connection.WriteBuffer(Value^, 8); // DimCount + Flags + ElementSize
    Connection.WriteBuffer(Value^.Bounds, 8 * Value.DimCount); // Bounds
    case Compress of
      False:
        Connection.WriteBuffer(DataPtr^, SafeArrayElementTotal(Value) * Value.ElementSize);
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
    end;
  finally
    SafeArrayUnaccessData(Value);
  end;
end;

procedure TJvUIBConnection.WritePWideChar(Value: PWideChar);
var
  StrCount: Integer;
begin
  StrCount := Length(Value) * 2 + 2;
  Connection.WriteBuffer(StrCount, SizeOf(StrCount));
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount);
end;

procedure TJvUIBConnection.WriteReal48(Value: Real48);
begin
  Connection.WriteBuffer(Value, SizeOf(Real48));
end;

procedure TJvUIBConnection.WriteShortint(Value: Shortint);
begin
  Connection.WriteBuffer(Value, SizeOf(Shortint));
end;

procedure TJvUIBConnection.WriteSingle(Value: Single);
begin
  Connection.WriteBuffer(Value, SizeOf(Single));
end;

procedure TJvUIBConnection.WriteSmallint(Value: Smallint);
begin
  Connection.WriteBuffer(Value, SizeOf(Smallint));
end;

procedure TJvUIBConnection.WriteStream(Stream: TStream; Compress: Boolean = False);
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
  end
  else
  begin
    WriteInteger(Stream.Size);
    Connection.WriteStream(Stream);
  end;
end;

procedure TJvUIBConnection.WriteString(Value: string);
var
  StrCount: Integer;
begin
  StrCount := Length(Value);
  Connection.WriteBuffer(StrCount, SizeOf(StrCount));
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount);
end;

procedure TJvUIBConnection.WriteVariant(var Value: Variant;
  Compress: Boolean);
begin
  Connection.WriteBuffer(TVarData(Value).VType, 2);
  // (rom) why not call the Write* methods here?
  case TVarData(Value).VType of
    varEmpty:
      {Nothing to do};
    varNull:
      {Nothing to do};
    varSmallInt:
      Connection.WriteBuffer(TVarData(Value).VSmallInt, SizeOf(Smallint));
    varInteger:
      Connection.WriteBuffer(TVarData(Value).VInteger, SizeOf(Integer));
    varSingle:
      Connection.WriteBuffer(TVarData(Value).VSingle, SizeOf(Single));
    varDouble:
      Connection.WriteBuffer(TVarData(Value).VDouble, SizeOf(Double));
    varCurrency:
      Connection.WriteBuffer(TVarData(Value).VCurrency,  SizeOf(Currency));
    varDate:
      Connection.WriteBuffer(TVarData(Value).VDate,  SizeOf(TDate));
    varOleStr:
      WritePWideChar(TVarData(Value).VOleStr);
    varBoolean:
      // (rom) Beware! Size inconsistency!
      Connection.WriteBuffer(TVarData(Value).VBoolean, 2);
    {$IFDEF DELPHI6_UP}
    varShortInt:
      Connection.WriteBuffer(TVarData(Value).VShortInt,  SizeOf(Shortint));
    varWord:
      Connection.WriteBuffer(TVarData(Value).VWord,  SizeOf(Word));
    varLongWord:
      Connection.WriteBuffer(TVarData(Value).VLongWord,  SizeOf(Longword));
    varInt64:
      Connection.WriteBuffer(TVarData(Value).VInt64,  SizeOf(Int64));
    {$ENDIF DELPHI6_UP}
    varByte:
      Connection.WriteBuffer(TVarData(Value).VByte,  SizeOf(Byte));
    varString:
      WritePChar(TVarData(Value).VString);
  else
    if TVarData(Value).VType and varArray = varArray then
      WritePVarArray(TVarData(Value).VArray, Compress)
    else
      raise EJvUIBException.Create(EJvUIB_DataType);
  end;
end;

procedure TJvUIBConnection.WriteWideString(Value: WideString);
var
  StrCount: Integer;
begin
  StrCount := Length(Value);
  Connection.WriteBuffer(StrCount,  SizeOf(StrCount));
  if StrCount > 0 then
    Connection.WriteBuffer(Value[1], StrCount *  SizeOf(WideChar));
end;

procedure TJvUIBConnection.WriteWord(Value: Word);
begin
  Connection.WriteBuffer(Value, SizeOf(Word));
end;

procedure TJvUIBConnection.WriteWordBool(Value: WordBool);
begin
  Connection.WriteBuffer(Value, SizeOf(WordBool));
end;

end.

