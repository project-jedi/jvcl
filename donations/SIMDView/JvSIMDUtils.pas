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
    XMMRegisters : TJvXMMRegisters;          // bytes from 160 to 287
    Reserved4    : array [416..511] of Byte; // bytes from 288 to 512
  end;

  TJvContext = packed record
     ScalarContext : Windows.TContext;
     VectorContext : TJvVectorFrame;
  end;

resourcestring
  RsVendorIntel = 'GenuineIntel';
  RsVendorAMD   = 'AuthenticAMD';

  RsMMX     = 'MMX';
  RsSSE1    = 'SSE1';
  RsSSE2    = 'SSE2';
  RsSSE3    = 'SSE3';
  Rs3DNow   = '3DNow!';
  RsExMMX   = 'Extensions to MMX';
  RsEx3DNow = 'Extensions to 3DNow!';
  RsLong    = '64-Bit';

  RsTrademarks = 'MMX is a trademark of Intel Corporation.'+sLineBreak+
                 '3DNow! is a registered trademark of Advanced Micro Devices.';
                 // SSE not registered by Intel Corp. ???? cannot find any reference
                 // to the registered name

  RsNo128BitSIMD = 'No 128-bit-wide-registers SIMD';

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
  TBitDescription = record
    BitCount: 0..2;
    ShortName: string; // only if BitCount>0
    LongName: string;  // only if BitCount>0
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

// return if the processor supports the CPUID instruction
function IsCPUIDSupported: Boolean;

type
  TCPUIDResult = record
    eaxValue : Cardinal;
    ebxValue : Cardinal;
    ecxValue : Cardinal;
    edxValue : Cardinal;
  end;

// execute a cpuid call with eax = Value
function CallCPUID (Value:Cardinal): TCPUIDResult;

type
  TProcessorVendor = array [0..11] of Char;

// return the processor brand see constants above
function ProcessorVendor: string;

type
  TVendorID = (viIntel, viAMD, viOther);

// return the processor vendorID
function ProcessorVendorID: TVendorID;

type
  _IntelSIMD = (isMMX, isSSE1, isSSE2, isSSE3);
  TIntelSIMD = set of _IntelSIMD;

// return the processor SSE version for intel processors
function IntelSIMD: TIntelSIMD;

type
  _AMDSIMD = (asMMX, as3DNow, asExMMX, asEx3DNow, asSSE1, asSSE2, asLong);
  TAMDSIMD = set of _AMDSIMD;

// return the processor 3DNow! version for AMD processors
function AMDSIMD: TAMDSIMD;

// return if the processor supports 64-bit-wide operands SIMD
function Is64BitSIMDPresent: Boolean;

// return if the processor supports 128-bit-wide operands SIMD
function Is128BitSIMDPresent: Boolean;

// return the SIMD version as a string
function SIMDString: string;

// process function calls to get the processor informations
procedure GetProcessorInfo;

implementation

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  SysUtils;

type
  TProcessorInfo = record
    CPUIDSupported: Boolean;
    Vendor: string;
    SIMDString: string;
    case VendorID: TVendorID of
      viIntel : (IntelSIMD: TIntelSIMD);
      viAMD   : (AMDSIMD: TAMDSIMD);
  end;

var
  ProcessorInfo: TProcessorInfo;
  ProcessorInfoValid: Boolean = False;

function GetThreadContext(hThread: THandle;
  var lpContext: TJvContext): BOOL;
  stdcall; external kernel32 name 'GetThreadContext';

function SetThreadContext(hThread: THandle;
  const lpContext: TJvContext): BOOL;
  stdcall; external kernel32 name 'SetThreadContext';

function CallCPUID (Value:Cardinal): TCPUIDResult;
// eax = Value
// edx = pointer to the result
asm
  push        ebx                               // save ebx value
  push        edi                               // save edi value
  mov         edi,        Result                // move destination to edi
//  mov         eax,        Value  // Value is in eax
  cpuid                                         // call cpuid with eax=Value
  mov         [edi].TCPUIDResult.eaxValue, eax  // save eax in the result record
  mov         [edi].TCPUIDResult.ebxValue, ebx  // save ebx in the result record
  mov         [edi].TCPUIDResult.ecxValue, ecx  // save ecx in the result record
  mov         [edi].TCPUIDResult.edxValue, edx  // save edx in the result record
  pop         edi                               // restore edi
  pop         ebx                               // restore ebx
end;

function IsCPUIDSupported: Boolean;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  Result := ProcessorInfo.CPUIDSupported;
end;

function GetIsCPUIDSupported: Boolean;
// according to Intel's and AMD's x86-processor documentation, CPUID instruction
// is supported if bit 21 of the EFlags register can be cleared and set
asm
  pushfd                                // push EFLAGS on the stack
  pop         eax                       // Get EFlags in eax
  mov         edx,        eax           // save eax in edx
  xor         eax,        (1 shl 21)    // toggle cpuid bit
  and         edx,        (1 shl 21)    // save only cpuid bit
  push        eax                       // push eax on the stack
  popfd                                 // save changes to eflags
  pushfd                                // push eflags on the stack
  pop         eax                       // get Eflags in eax
  mov         ecx,        eax           // save eax in ecx
  and         eax,        (1 shl 21)    // save only cpuid bit
  xor         eax,        edx           // test if cpuid bit is changed
  jz          @@NoCPUID                 // if zero, CPUID is not supported
  xor         ecx,        (1 shl 21)    // toggle cpuid bit
  push        ecx                       // push ecx on the stack
  popfd                                 // save changes to eflags
  pushfd                                // push eflags on the stack
  pop         eax                       // get eflags in eax
  and         eax,        (1 shl 21)    // only cpuid bit
  xor         eax,        edx           // test if cpuid bit is the same than in the beginning
  jnz         @@NoCPUID                 // if different, CPUID is not supported
  mov         Result,     True          // CPUID is supported
  jmp         @@CPUIDEnd
@@NoCPUID:
  mov         Result,     False         // CPUID is not supported
@@CPUIDEnd:
end;

function ProcessorVendor: string;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  Result := ProcessorInfo.Vendor;
end;

function GetProcessorVendor: string;
// when calling cpuid with eax=0, Vendor is return in the string [ebx,edx,ecx]
begin
  SetLength(Result,12);
  with CallCPUID(0) do
  begin
    CopyMemory(@Result[1],@ebxValue,4);
    CopyMemory(@Result[5],@edxValue,4);
    CopyMemory(@Result[9],@ecxValue,4);
  end;
end;

function ProcessorVendorID: TVendorID;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  Result := ProcessorInfo.VendorID;
end;

function GetProcessorVendorID: TVendorID;
var
  VendorName: string;
begin
  VendorName := ProcessorVendor;
  if CompareText(VendorName,RsVendorIntel)=0 then
    Result := viIntel
  else if CompareText(VendorName,RsVendorAMD)=0 then
    Result := viAMD
  else Result := viOther;
end;

function IntelSIMD: TIntelSIMD;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  Result := ProcessorInfo.IntelSIMD;
end;

function GetIntelSIMD: TIntelSIMD;
var
  FeatureInformations: Cardinal;
  ExFeatureInformations: Cardinal;
begin
  Result := [];
  if CallCPUID(0).eaxValue >= 1 then
    with CallCPUID(1) do
  begin
    FeatureInformations := edxValue;
    ExFeatureInformations := ecxValue;
    if (FeatureInformations and (1 shl 23))<>0 then
      Include(Result,isMMX);
    if (FeatureInformations and (1 shl 25))<>0 then
      Include(Result,isSSE1);
    if (FeatureInformations and (1 shl 26))<>0 then
      Include(Result,isSSE2);
    if (ExFeatureInformations and 1)<>0 then
      Include(Result,isSSE3);
  end;
end;

function GetIntelSIMDString: string;
  function AppendStr(Left, Right: string): string;
  begin
    if Left = '' then
      Result := ''
    else
      Result := Left+',';
    Result := Result + Right;
  end;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  with ProcessorInfo do
  begin
    Result := '';
    if (VendorID <> viIntel) then
      Exit;
    if (isMMX in IntelSIMD) then
      Result := AppendStr(Result,RsMMX);
    if (isSSE1 in IntelSIMD) then
      Result := AppendStr(Result,RsSSE1);
    if (isSSE2 in IntelSIMD) then
      Result := AppendStr(Result,RsSSE2);
    if (isSSE3 in IntelSIMD) then
      Result := AppendStr(Result,RsSSE3);
  end;
end;

function AMDSIMD: TAMDSIMD;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  Result := ProcessorInfo.AMDSIMD;
end;

function GetAMDSIMD: TAMDSIMD;
var
  StandardFeatureSupports: Cardinal;
  AMDFeatureSupports: Cardinal;
begin
  Result := [];
  if CallCPUID(0).eaxValue >= 1 then
  with CallCPUID(1) do
  begin
    StandardFeatureSupports := edxValue;
    if (StandardFeatureSupports and (1 shl 23))<>0 then
      Include(Result,asMMX);
    if (StandardFeatureSupports and (1 shl 25))<>0 then
      Include(Result,asSSE1);
    if (StandardFeatureSupports and (1 shl 26))<>0 then
      Include(Result,asSSE2);
  end;
  if CallCPUID($80000000).eaxValue >= $80000001 then
    with CallCPUID($80000001) do
  begin
    AMDFeatureSupports := edxValue;
    if (AMDFeatureSupports and (1 shl 22))<>0 then
      Include(Result,asExMMX);
    if (AMDFeatureSupports and (1 shl 29))<>0 then
      Include(Result,asLong);
    if (AMDFeatureSupports and (1 shl 30))<>0 then
      Include(Result,asEx3DNow);
    if (AMDFeatureSupports and (1 shl 31))<>0 then
      Include(Result,as3DNow);
  end;
end;

function Is128BitSIMDPresent: Boolean;
// SSE and 3dNow! are 128-bit-wide operands
begin
  case ProcessorVendorID of
    viIntel : Result := (IntelSIMD * [isSSE1, isSSE2, isSSE3])<>[];
    viAMD   : Result := (AMDSIMD * [as3DNow, asEx3DNow, asSSE1, asSSE2])<>[];
    //viOther,
    else      Result := False;
  end;
end;

function Is64BitSIMDPresent: Boolean;
// MMX are 64-bit-wide operands
begin
  case ProcessorVendorID of
    viIntel : Result := isMMX in IntelSIMD;
    viAMD   : Result := (AMDSIMD * [asMMX, asExMMX])<>[];
    //viOther,
    else      Result := False;
  end;
end;

function GetAMDSIMDString: string;
  function AppendStr(Left, Right: string): string;
  begin
    if Left = '' then
      Result := ''
    else
      Result := Left+',';
    Result := Result + Right;
  end;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  with ProcessorInfo do
  begin
    Result := '';
    if (VendorID <> viAMD) then
      Exit;
    if (asMMX in AMDSIMD) then
      Result := AppendStr(Result,RsMMX);
    if (as3DNow in AMDSIMD) then
      Result := AppendStr(Result,Rs3DNow);
    if (asExMMX in AMDSIMD) then
      Result := AppendStr(Result,RsExMMX);
    if (asEx3DNow in AMDSIMD) then
      Result := AppendStr(Result,RsEx3DNow);
    if (asSSE1 in AMDSIMD) then
      Result := AppendStr(Result,RsSSE1);
    if (asSSE2 in AMDSIMD) then
      Result := AppendStr(Result,RsSSE2);
    if (asLong in AMDSIMD) then
      Result := AppendStr(Result,RsLong);
  end;
end;

function SIMDString: string;
begin
  if (not ProcessorInfoValid) then
    GetProcessorInfo;
  Result := ProcessorInfo.SIMDString;
end;

procedure GetProcessorInfo;
begin
  FillMemory(@ProcessorInfo,SizeOf(ProcessorInfo),0);
  ProcessorInfoValid := True;
  with ProcessorInfo do
  begin
    CPUIDSupported := GetIsCPUIDSupported;
    if (CPUIDSupported) then
    begin
      Vendor := GetProcessorVendor;
      VendorID := GetProcessorVendorID;
      case VendorID of
        viIntel :
          begin
            IntelSIMD := GetIntelSIMD;
            SIMDString := GetIntelSIMDString;
          end;
        viAMD   :
          begin
            AMDSIMD := GetAMDSIMD;
            SIMDString := GetAMDSIMDString;
          end;
      end;
      if (SIMDString = '') then
        SIMDString := RsNo128BitSIMD;
    end;
  end;
end;

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
