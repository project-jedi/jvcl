{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDUtils.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvSIMDUtils;

{$I jvcl.inc}

interface

uses
  Windows;

resourcestring
  RsSSE     = 'SSE';
  RsMMX     = 'MMX';
  RsSSE1    = 'SSE1';
  RsSSE2    = 'SSE2';
  RsSSE3    = 'SSE3';
  Rs3DNow   = '3DNow!';
  RsExMMX   = 'Extensions to MMX';
  RsEx3DNow = 'Extensions to 3DNow!';
  RsLong    = '64-bit Core';

  RsTrademarks = 'MMX is a trademark of Intel Corporation.'+sLineBreak+
                 '3DNow! is a registered trademark of Advanced Micro Devices.';

  RsNoSIMD    = 'No SIMD registers found';
  RsNoSSE     = 'SSE are not supported on this processor';
  RsNo128SIMD = 'No 128-bit-register SIMD';
  RsNo64SIMD  = 'No 64-bit-register SIMD';

  RsVectorIE  = 'IE  ';
  RsVectorDE  = 'DE  ';
  RsVectorZE  = 'ZE  ';
  RsVectorOE  = 'OE  ';
  RsVectorUE  = 'UE  ';
  RsVectorPE  = 'PE  ';
  RsVectorDAZ = 'DAZ ';   //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIM  = 'IM  ';
  RsVectorDM  = 'DM  ';
  RsVectorZM  = 'ZM  ';
  RsVectorOM  = 'OM  ';
  RsVectorUM  = 'UM  ';
  RsVectorPM  = 'PM  ';
  RsVectorRC  = 'RC  ';
  RsVectorFZ  = 'FZ  ';

  RsVectorIEText  = 'Invalid-operation exception';
  RsVectorDEText  = 'Denormal-operand exception';
  RsVectorZEText  = 'Zero-divide exception';
  RsVectorOEText  = 'Overflow exception';
  RsVectorUEText  = 'Underflow exception';
  RsVectorPEText  = 'Precision exception';
  RsVectorDAZText = 'Denormal are zeros';      //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIMText  = 'Invalid-operation mask';
  RsVectorDMText  = 'Denormal-operand mask';
  RsVectorZMText  = 'Zero-divide mask';
  RsVectorOMText  = 'Overflow mask';
  RsVectorUMText  = 'Underflow mask';
  RsVectorPMText  = 'Precision mask';
  RsVectorRCText  = 'Rounding control';
  RsVectorFZText  = 'Flush to zero';

  RsRoundToNearest  = 'Round to nearest';
  RsRoundDown       = 'Round down';
  RsRoundUp         = 'Round up';
  RsRoundTowardZero = 'Round toward zero';

type
  TJvMMContentType = (mt8Bytes, mt4Words, mt2DWords);

  TJVMMRegister = packed record
    case TJvMMContentType of
      mt8Bytes  : ( Bytes  : array [0..7] of Byte; );
      mt4Words  : ( Words  : array [0..3] of Word; );
      mt2DWords : ( DWords : array [0..1] of Cardinal; );
  end;

  TJvFPUContentType = (ftFloat, ftSimd);

  TJvFPUData = packed record
    case TJvFPUContentType of
      ftFloat : ( FloatValue : Extended; );
      ftSimd  : ( MMRegister : TJvMMRegister;
                  Reserved   : Word );
  end;

  TJvFPURegister = packed record
    Data     : TJvFPUData;
    Reserved : array [0..5] of Byte;
  end;

  TJvFPURegisters = array [0..7] of TJvFPURegister;

  TJvXMMContentType = (xt16Bytes, xt8Words, xt4DWords,
                       xt2QWords, xt4Singles, xt2Doubles);

  TJvXMMRegister = packed record
    case TJvXMMContentType of
      xt16Bytes  : ( Bytes   : array [0..15] of Byte;    );
      xt8Words   : ( Words   : array [0..7] of Word;     );
      xt4DWords  : ( DWords  : array [0..3] of Cardinal; );
      xt2QWords  : ( QWords  : array [0..1] of Int64;    );
      xt4Singles : ( Singles : array [0..3] of Single;   );
      xt2Doubles : ( Doubles : array [0..1] of Double;   );
  end;

  TJvProcessorSize = (ps32Bits, ps64Bits);

  TJvXMMRegisters = packed record
    case TJvProcessorSize of
      ps32Bits : ( LegacyXMM      : array [0..7] of TJvXMMRegister;
                   LegacyReserved : array [0..127] of Byte; );
      ps64Bits : ( LongXMM        : array [0..15] of TJvXMMRegister; );
  end;

  TJvMXCSRBits = ( mbInvalidOperationException,  // = 0
                   mbDenormalException,          // = 1
                   mbDivideByZeroException,      // = 2
                   mbOverflowException,          // = 3
                   mbUnderflowException,         // = 4
                   mbPrecisionException,         // = 5
                   mbDenormalsAreZeros,          // = 6 (Only in Intel P4, Intel Xeon and AMD)
                   mbInvalidOperationMask,       // = 7
                   mbDenormalMask,               // = 8
                   mbDivideByZeroMask,           // = 9
                   mbOverflowMask,               // = 10
                   mbUnderflowMask,              // = 11
                   mbPrecisionMask,              // = 12
                   mbRoundingControl1,           // = 13
                   mbRoundingControl2,           // = 14
                   mbFlushToZero,                // = 15
                   mbReserved1,                  // = 16
                   mbReserved2,                  // = 17
                   mbReserved3,                  // = 18
                   mbReserved4,                  // = 19
                   mbReserved5,                  // = 20
                   mbReserved6,                  // = 21
                   mbReserved7,                  // = 22
                   mbReserved8,                  // = 23
                   mbReserved9,                  // = 24
                   mbReserved10,                 // = 25
                   mbReserved11,                 // = 26
                   mbReserved12,                 // = 27
                   mbReserved13,                 // = 28
                   mbReserved14,                 // = 29
                   mbReserved15,                 // = 30
                   mbReserved16);                // = 31

  TJvRoundingControl = (rcRoundToNearest,   //=0
                        rcRoundDown,        //=1
                        rcRoundUp,          //=2
                        rcRoundTowardZero); //=3

  TJvMXCSR = set of TJvMXCSRBits;

  TJvVectorFrame = packed record
    FCW          : Word;                     // bytes from 0   to 1
    FSW          : Word;                     // bytes from 2   to 3
    FTW          : Byte;                     // byte 4
    Reserved1    : Byte;                     // byte 5
    FOP          : Word;                     // bytes from 6   to 7
    FpuIp        : Cardinal;                 // bytes from 8   to 11
    CS           : Word;                     // bytes from 12  to 13
    Reserved2    : Word;                     // bytes from 14  to 15
    FpuDp        : Cardinal;                 // bytes from 16  to 19
    DS           : Word;                     // bytes from 20  to 21
    Reserved3    : Word;                     // bytes from 22  to 23
    MXCSR        : TJvMXCSR;                 // bytes from 24  to 27
    MXCSRMask    : TJvMXCSR;                 // bytes from 28  to 31
    FPURegisters : TJvFPURegisters;          // bytes from 32  to 159
    XMMRegisters : TJvXMMRegisters;          // bytes from 160 to 415
    Reserved4    : array [416..511] of Byte; // bytes from 416 to 512
  end;

  TJvContext = packed record
     ScalarContext : Windows.TContext;
     VectorContext : TJvVectorFrame;
  end;

type
  TBitDescription = record
    BitCount: 0..2;
    ShortName: string;            // only if BitCount>0
    LongName: string;             // only if BitCount>0
    Names: array [0..3] of string // only if BitCount=2
  end;

const
  MXCSRBitsDescription : array [mbInvalidOperationException..mbFlushToZero] of TBitDescription =
    (
      (BitCount:1; ShortName:RsVectorIE;  LongName:RsVectorIEText),   //mbInvalidOperationException
      (BitCount:1; ShortName:RsVectorDE;  LongName:RsVectorDEText),   //mbDenormalException
      (BitCount:1; ShortName:RsVectorZE;  LongName:RsVectorZEText),   //mbDivideByZeroException
      (BitCount:1; ShortName:RsVectorOE;  LongName:RsVectorOEText),   //mbOverflowException
      (BitCount:1; ShortName:RsVectorUE;  LongName:RsVectorUEText),   //mbUnderflowException
      (BitCount:1; ShortName:RsVectorPE;  LongName:RsVectorPEText),   //mbPrecisionException
      (BitCount:1; ShortName:RsVectorDAZ; LongName:RsVectorDAZText),  //mbDenormalsAreZeros
      (BitCount:1; ShortName:RsVectorIM;  LongName:RsVectorIMText),   //mbInvalidOperationMask
      (BitCount:1; ShortName:RsVectorDM;  LongName:RsVectorDMText),   //mbDenormalMask
      (BitCount:1; ShortName:RsVectorZM;  LongName:RsVectorZMText),   //mbDivideByZeroMask
      (BitCount:1; ShortName:RsVectorOM;  LongName:RsVectorOMText),   //mbOverflowMask
      (BitCount:1; ShortName:RsVectorUM;  LongName:RsVectorUMText),   //mbUnderflowMask
      (BitCount:1; ShortName:RsVectorPM;  LongName:RsVectorPMText),   //mbPrecisionMask
      (BitCount:2; ShortName:RsVectorRC;  LongName:RsVectorRCText; Names:(RsRoundToNearest,
                                                                          RsRoundDown,
                                                                          RsRoundUp,
                                                                          RsRoundTowardZero)), //mbRoundingControl1
      (BitCount:0),                                                   //mbRoundingControl2
      (BitCount:1; ShortName:RsVectorFZ;  LongName:RsVectorFZText)    //mbFlushToZero
    );

type
  TJvSIMDDisplay = ( sdBytes, sdWords, sdDWords, sdQWords, sdSingles, sdDoubles );

  TJvSIMDValue = packed record
    case Display: TJvSIMDDisplay of
      sdBytes   : (ValueByte: Byte;      );
      sdWords   : (ValueWord: Word;      );
      sdDWords  : (ValueDWord: Cardinal; );
      sdQWords  : (ValueQWord: Int64;    );
      sdSingles : (ValueSingle: Single;  );
      sdDoubles : (ValueDouble: Double;  );
  end;

  TJvSIMDFormat = ( sfBinary, sfSigned, sfUnsigned, sfHexa );

function FormatValue(Value:TJvSIMDValue; Format: TJvSIMDFormat):string;

const
  CONTEXT_EXTENDED_REGISTERS = CONTEXT_i386 or $20;

// return the processor frame for the specified thread, this thread must be suspended
function GetThreadContext(hThread: THandle;
  var lpContext: TJvContext): BOOL; stdcall;

// set the processor frame for the specified thread, this thread must be suspended
function SetThreadContext(hThread: THandle;
  const lpContext: TJvContext): BOOL; stdcall;

// return the XMM registers for the specified thread, this thread must be suspended
function GetVectorContext(hThread: THandle; out VectorContext:TJvVectorFrame): Boolean;
// return the XMM registers for the specified thread, this thread must be suspended
function SetVectorContext(hThread: THandle; const VectorContext:TJvVectorFrame): Boolean;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils;

function FormatBinary(Value: TJvSIMDValue): string;
var
  I:Byte;
const
  Width: array [sdBytes..sdQWords] of Byte = (8, 16, 32, 64);
begin
  Assert(Value.Display<sdSingles);
  Result:=StringOfChar('0',Width[Value.Display]);
  for I:=1 to Width[Value.Display] do
  begin
    if (Value.ValueQWord and 1)<>0
      then Result[Width[Value.Display]-I+1]:='1';
    Value.ValueQWord:=Value.ValueQWord shr 1;
  end;
end;

function FormatSigned(Value: TJvSIMDValue): string;
const
  Width: array [sdBytes..sdQWords] of Byte = (4, 6, 11, 20);
begin
  Assert(Value.Display<sdSingles);
  case Value.Display of
    sdBytes  : Result:=IntToStr(ShortInt(Value.ValueByte));
    sdWords  : Result:=IntToStr(SmallInt(Value.ValueWord));
    sdDWords : Result:=IntToStr(Integer(Value.ValueDWord));
    sdQWords : Result:=IntToStr(Value.ValueQWord);
    else       Result := '';
  end;
  Result:=StringOfChar(' ',Width[Value.Display]-Length(Result))+Result;
end;

function FormatUnsigned(Value: TJvSIMDValue): string;
const
  Width: array [sdBytes..sdQWords] of Byte = (3, 5, 10, 20);
begin
  Assert(Value.Display<sdSingles);
  case Value.Display of
    sdBytes  : Result:=IntToStr(Byte(Value.ValueByte));
    sdWords  : Result:=IntToStr(Word(Value.ValueWord));
    sdDWords : Result:=IntToStr(Cardinal(Value.ValueDWord));
    sdQWords : Result:=IntToStr(Value.ValueQWord);
    else       Result := '';
  end;
  Result:=StringOfChar(' ',Width[Value.Display]-Length(Result))+Result;
end;

function FormatHexa(Value: TJvSIMDValue): string;
const
  Width: array [sdBytes..sdQWords] of Byte = (2, 4, 8, 16);
begin
  Assert(Value.Display<sdSingles);
  case Value.Display of
    sdBytes  : Result := IntToHex(Value.ValueByte,Width[sdBytes]);
    sdWords  : Result := IntToHex(Value.ValueWord,Width[sdWords]);
    sdDWords : Result := IntToHex(Value.ValueDWord,Width[sdDWords]);
    sdQWords : Result := IntToHex(Value.ValueQWord,Width[sdQWords]);
    else       Result := '';
  end;
end;

function FormatFloat(Value:TJvSIMDValue): string;
begin
  Assert(Value.Display>=sdSingles);
  case Value.Display of
    sdSingles : Result := FloatToStr(Value.ValueSingle);
    sdDoubles : Result := FloatToStr(Value.ValueDouble);
    else        Result := '';
  end;
  Result:=StringOfChar(' ',22-Length(Result))+Result;     // 22 = max string length of a double value
end;

function FormatValue(Value:TJvSIMDValue; Format: TJvSIMDFormat): string;
type
  TFormatFunction = function (Value:TJvSIMDValue): string;
var
  FormatFunction: TFormatFunction;
begin
  Result := '';
  case Format of
    sfBinary   : FormatFunction := FormatBinary;
    sfSigned   : FormatFunction := FormatSigned;
    sfUnsigned : FormatFunction := FormatUnsigned;
    sfHexa     : FormatFunction := FormatHexa;
    else         Exit;
  end;
  case Value.Display of
    sdBytes..sdQWords    : Result := FormatFunction(Value);
    sdSingles..sdDoubles : Result := FormatFloat(Value);
  end;
end;

function GetThreadContext(hThread: THandle;
  var lpContext: TJvContext): BOOL;
  stdcall; external kernel32 name 'GetThreadContext';

function SetThreadContext(hThread: THandle;
  const lpContext: TJvContext): BOOL;
  stdcall; external kernel32 name 'SetThreadContext';

function GetVectorContext(hThread: THandle; out VectorContext:TJvVectorFrame): Boolean;
var
  JvContext:TJvContext;
begin
  JvContext.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
  Result :=    GetThreadContext(hThread,JvContext)
           and ((JvContext.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS)<>0);
  VectorContext := JvContext.VectorContext;
end;

function SetVectorContext(hThread: THandle; const VectorContext:TJvVectorFrame): Boolean;
var
  JvContext:TJvContext;
begin
  JvContext.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
  Result :=    GetThreadContext(hThread,JvContext)
           and ((JvContext.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS)<>0);
  if (Result) then
  begin
    JvContext.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
    JvContext.VectorContext := VectorContext;
    Result := SetThreadContext(hThread,JvContext);
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
