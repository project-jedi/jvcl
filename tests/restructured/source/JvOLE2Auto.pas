{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvOle2Auto.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


unit JvOle2Auto;

interface

{$IFDEF WIN32}
uses Windows, SysUtils, {$IFDEF COMPILER3_UP} ActiveX, ComObj {$ELSE}
  Ole2, OleAuto, OleCtl {$ENDIF};
{$ELSE}
uses WinTypes, WinProcs, SysUtils, Ole2, Dispatch;
{$ENDIF}

const { Maximum number of dispatch arguments }
{$IFDEF COMPILER3_UP}
  MaxDispArgs = 64;
{$ELSE}
  MaxDispArgs = 32;
{$ENDIF}

{$IFNDEF WIN32}
type
  TDispID = DISPID;
  PDispID = ^TDispID;
  TDispParams = DISPPARAMS;
  TLCID = LCID;
  TExcepInfo = EXCEPINFO;
  PDispIDList = ^TDispIDList;
  TDispIDList = array[0..MaxDispArgs] of TDispID;
  EOleError = class(EJVCLException);
{$ENDIF WIN32}

{$IFNDEF COMPILER3_UP}
type
  EPropReadOnly = class(EOleError);
  EPropWriteOnly = class(EOleError);
{$ENDIF}

{$IFNDEF WIN32}

const
{ Primary language IDs. }
  LANG_NEUTRAL                     = $00;

  LANG_AFRIKAANS                   = $36;
  LANG_ALBANIAN                    = $1C;
  LANG_ARABIC                      = $01;
  LANG_BASQUE                      = $2D;
  LANG_BELARUSIAN                  = $23;
  LANG_BULGARIAN                   = $02;
  LANG_CATALAN                     = $03;
  LANG_CHINESE                     = $04;
  LANG_CROATIAN                    = $1A;
  LANG_CZECH                       = $05;
  LANG_DANISH                      = $06;
  LANG_DUTCH                       = $13;
  LANG_ENGLISH                     = $09;
  LANG_ESTONIAN                    = $25;
  LANG_FAEROESE                    = $38;
  LANG_FARSI                       = $29;
  LANG_FINNISH                     = $0B;
  LANG_FRENCH                      = $0C;
  LANG_GERMAN                      = $07;
  LANG_GREEK                       = $08;
  LANG_HEBREW                      = $0D;
  LANG_HUNGARIAN                   = $0E;
  LANG_ICELANDIC                   = $0F;
  LANG_INDONESIAN                  = $21;
  LANG_ITALIAN                     = $10;
  LANG_JAPANESE                    = $11;
  LANG_KOREAN                      = $12;
  LANG_LATVIAN                     = $26;
  LANG_LITHUANIAN                  = $27;
  LANG_NORWEGIAN                   = $14;
  LANG_POLISH                      = $15;
  LANG_PORTUGUESE                  = $16;
  LANG_ROMANIAN                    = $18;
  LANG_RUSSIAN                     = $19;
  LANG_SERBIAN                     = $1A;
  LANG_SLOVAK                      = $1B;
  LANG_SLOVENIAN                   = $24;
  LANG_SPANISH                     = $0A;
  LANG_SWEDISH                     = $1D;
  LANG_THAI                        = $1E;
  LANG_TURKISH                     = $1F;
  LANG_UKRAINIAN                   = $22;
  LANG_VIETNAMESE                  = $2A;

{ Sublanguage IDs. }
  SUBLANG_NEUTRAL                  = $00;    { language neutral }
  SUBLANG_DEFAULT                  = $01;    { user default }
  SUBLANG_SYS_DEFAULT              = $02;    { system default }

  SUBLANG_CHINESE_TRADITIONAL      = $01;    { Chinese (Taiwan) }
  SUBLANG_CHINESE_SIMPLIFIED       = $02;    { Chinese (PR China) }
  SUBLANG_CHINESE_HONGKONG         = $03;    { Chinese (Hong Kong) }
  SUBLANG_CHINESE_SINGAPORE        = $04;    { Chinese (Singapore) }
  SUBLANG_DUTCH                    = $01;    { Dutch }
  SUBLANG_DUTCH_BELGIAN            = $02;    { Dutch (Belgian) }
  SUBLANG_ENGLISH_US               = $01;    { English (USA) }
  SUBLANG_ENGLISH_UK               = $02;    { English (UK) }
  SUBLANG_ENGLISH_AUS              = $03;    { English (Australian) }
  SUBLANG_ENGLISH_CAN              = $04;    { English (Canadian) }
  SUBLANG_ENGLISH_NZ               = $05;    { English (New Zealand) }
  SUBLANG_ENGLISH_EIRE             = $06;    { English (Irish) }
  SUBLANG_FRENCH                   = $01;    { French }
  SUBLANG_FRENCH_BELGIAN           = $02;    { French (Belgian) }
  SUBLANG_FRENCH_CANADIAN          = $03;    { French (Canadian) }
  SUBLANG_FRENCH_SWISS             = $04;    { French (Swiss) }
  SUBLANG_GERMAN                   = $01;    { German }
  SUBLANG_GERMAN_SWISS             = $02;    { German (Swiss) }
  SUBLANG_GERMAN_AUSTRIAN          = $03;    { German (Austrian) }
  SUBLANG_ITALIAN                  = $01;    { Italian }
  SUBLANG_ITALIAN_SWISS            = $02;    { Italian (Swiss) }
  SUBLANG_NORWEGIAN_BOKMAL         = $01;    { Norwegian (Bokmal) }
  SUBLANG_NORWEGIAN_NYNORSK        = $02;    { Norwegian (Nynorsk) }
  SUBLANG_PORTUGUESE               = $02;    { Portuguese }
  SUBLANG_PORTUGUESE_BRAZILIAN     = $01;    { Portuguese (Brazilian) }
  SUBLANG_SPANISH                  = $01;    { Spanish (Castilian) }
  SUBLANG_SPANISH_MEXICAN          = $02;    { Spanish (Mexican) }
  SUBLANG_SPANISH_MODERN           = $03;    { Spanish (Modern) }

{ Default System and User IDs for language and locale. }
  LANG_SYSTEM_DEFAULT   = (SUBLANG_SYS_DEFAULT shl 10) or LANG_NEUTRAL;
  LANG_USER_DEFAULT     = (SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL;
  LOCALE_SYSTEM_DEFAULT = (0 shl 16) or LANG_SYSTEM_DEFAULT;
  LOCALE_USER_DEFAULT   = (0 shl 16) or LANG_USER_DEFAULT;

{ OLE control status codes }
  CTL_E_ILLEGALFUNCTIONCALL       = $800A0000 + 5;
  CTL_E_OVERFLOW                  = $800A0000 + 6;
  CTL_E_OUTOFMEMORY               = $800A0000 + 7;
  CTL_E_DIVISIONBYZERO            = $800A0000 + 11;
  CTL_E_OUTOFSTRINGSPACE          = $800A0000 + 14;
  CTL_E_OUTOFSTACKSPACE           = $800A0000 + 28;
  CTL_E_BADFILENAMEORNUMBER       = $800A0000 + 52;
  CTL_E_FILENOTFOUND              = $800A0000 + 53;
  CTL_E_BADFILEMODE               = $800A0000 + 54;
  CTL_E_FILEALREADYOPEN           = $800A0000 + 55;
  CTL_E_DEVICEIOERROR             = $800A0000 + 57;
  CTL_E_FILEALREADYEXISTS         = $800A0000 + 58;
  CTL_E_BADRECORDLENGTH           = $800A0000 + 59;
  CTL_E_DISKFULL                  = $800A0000 + 61;
  CTL_E_BADRECORDNUMBER           = $800A0000 + 63;
  CTL_E_BADFILENAME               = $800A0000 + 64;
  CTL_E_TOOMANYFILES              = $800A0000 + 67;
  CTL_E_DEVICEUNAVAILABLE         = $800A0000 + 68;
  CTL_E_PERMISSIONDENIED          = $800A0000 + 70;
  CTL_E_DISKNOTREADY              = $800A0000 + 71;
  CTL_E_PATHFILEACCESSERROR       = $800A0000 + 75;
  CTL_E_PATHNOTFOUND              = $800A0000 + 76;
  CTL_E_INVALIDPATTERNSTRING      = $800A0000 + 93;
  CTL_E_INVALIDUSEOFNULL          = $800A0000 + 94;
  CTL_E_INVALIDFILEFORMAT         = $800A0000 + 321;
  CTL_E_INVALIDPROPERTYVALUE      = $800A0000 + 380;
  CTL_E_INVALIDPROPERTYARRAYINDEX = $800A0000 + 381;
  CTL_E_SETNOTSUPPORTEDATRUNTIME  = $800A0000 + 382;
  CTL_E_SETNOTSUPPORTED           = $800A0000 + 383;
  CTL_E_NEEDPROPERTYARRAYINDEX    = $800A0000 + 385;
  CTL_E_SETNOTPERMITTED           = $800A0000 + 387;
  CTL_E_GETNOTSUPPORTEDATRUNTIME  = $800A0000 + 393;
  CTL_E_GETNOTSUPPORTED           = $800A0000 + 394;
  CTL_E_PROPERTYNOTFOUND          = $800A0000 + 422;
  CTL_E_INVALIDCLIPBOARDFORMAT    = $800A0000 + 460;
  CTL_E_INVALIDPICTURE            = $800A0000 + 481;
  CTL_E_PRINTERERROR              = $800A0000 + 482;
  CTL_E_CANTSAVEFILETOTEMP        = $800A0000 + 735;
  CTL_E_SEARCHTEXTNOTFOUND        = $800A0000 + 744;
  CTL_E_REPLACEMENTSTOOLONG       = $800A0000 + 746;
  CTL_E_CUSTOM_FIRST              = $800A0000 + 600;

{$ENDIF WIN32}

type

{ OLE2 Automation Controller }

  TJvOleController = class(TObject)
  private
    FLocale: TLCID;
    FObject: Variant;
    FRetValue: Variant;
    function CallMethod(ID: TDispID; const Params: array of const;
      NeedResult: Boolean): PVariant;
    function CallMethodNamedParams(const IDs: TDispIDList;
      const Params: array of const; Cnt: Byte; NeedResult: Boolean): PVariant;
    function CallMethodNoParams(ID: TDispID; NeedResult: Boolean): PVariant;
    function Invoke(dispidMember: TDispID; wFlags: Word;
      var pdispparams: TDispParams; Res: PVariant): PVariant;
    function NameToDispID(const AName: string): TDispID;
    function NameToDispIDs(const AName: string;
      const AParams: array of string; Dest: PDispIDList): PDispIDList;
  protected
    procedure ClearObject; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { create or assign OLE objects }
    procedure CreateObject(const ClassName: string); virtual;
    procedure AssignIDispatch(V: Variant); virtual;
    procedure GetActiveObject(const ClassName: string); virtual;
    { get/set properties of OLE object by ID }
    function GetPropertyByID(ID: TDispID): PVariant;
    procedure SetPropertyByID(ID: TDispID; const Prop: array of const);
    { get/set properties of OLE object }
    function GetProperty(const AName: string): PVariant;
    procedure SetProperty(const AName: string; const Prop: array of const);
    { call OLE functions by IDs }
    function CallFunctionByID(ID: TDispID; const Params: array of const): PVariant;
    function CallFunctionByIDsNamedParams(const IDs: TDispIDList;
      const Params: array of const; Cnt: Byte): PVariant;
    function CallFunctionNoParamsByID(ID: TDispID): PVariant;
    { call OLE procedures by ID }
    procedure CallProcedureByID(ID: TDispID; const Params: array of const);
    procedure CallProcedureByIDsNamedParams(const IDs: TDispIDList;
      const Params: array of const; Cnt: Byte);
    procedure CallProcedureNoParamsByID(ID: TDispID);
    { call OLE functions }
    function CallFunction(const AName: string; const Params: array of const): PVariant;
    function CallFunctionNamedParams(const AName: string; const Params: array of const;
      const ParamNames: array of string): PVariant;
    function CallFunctionNoParams(const AName: string): PVariant;
    { call OLE procedures }
    procedure CallProcedure(const AName: string; const Params: array of const);
    procedure CallProcedureNamedParams(const AName: string; const Params: array of const;
      const ParamNames: array of string);
    procedure CallProcedureNoParams(const AName: string);
    { locale }
    procedure SetLocale(PrimaryLangID, SubLangID: Word);
    property Locale: TLCID read FLocale write FLocale;
    property OleObject: Variant read FObject;
  end;

procedure InitOLE;
procedure DoneOLE;
function OleInitialized: Boolean;

function MakeLangID(PrimaryLangID, SubLangID: Word): Word;
function MakeLCID(LangID: Word): TLCID;
function CreateLCID(PrimaryLangID, SubLangID: Word): TLCID;
function ExtractLangID(LCID: TLCID): Word;
function ExtractSubLangID(LCID: TLCID): Word;

{$IFNDEF WIN32}

procedure OleCheck(OleResult: HResult);

{ OLE string support }
function OleStrToString(Source: BSTR): string;
function StringToOleStr(const Source: string): BSTR;
function StringToClassID(const S: string): CLSID;
function ClassIDToString(const CLSID: CLSID): string;

{ Create or get active OLE object for a given a class name }
function CreateOleObject(const ClassName: string): Variant;
function GetActiveOleObject(const ClassName: string): Variant;

{$ENDIF WIN32}

implementation

uses Forms;

{$IFDEF COMPILER3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SOleInvalidVer   = 'Invalid OLE library version';
  SOleInitFailed   = 'OLE Library initialization failed. Error code: %.8xH';
  SOleNotInit      = 'OLE2 Library not initialized';
  SOleInvalidParam = 'Invalid parameter value';
  SOleNotSupport   = 'Method or property %s not supported by OLE object';
  SOleNotReference = 'Variant does not reference an OLE automation object';
{$IFNDEF COMPILER3_UP}
  SOleError        = 'OLE2 error occurred. Error code: %.8xH';
{$ENDIF}

const
  FOleInitialized: Boolean = False;

const
{ OLE2 Version }
  RMJ =   0;
  RMM =  23;
  RUP = 639;

const
  DISPATCH_METHODNOPARAM = DISPATCH_METHOD or DISPATCH_PROPERTYGET;
  DISPATCH_METHODPARAMS = DISPATCH_METHOD
    {$IFDEF WIN32} or DISPATCH_PROPERTYGET {$ENDIF};

{$IFDEF WIN32}

function FailedHR(hr: HResult): Boolean;
begin
  Result := Failed(hr);
end;

{$ELSE WIN32}

{ Standard OLE class pathes }

type
  IDispatch = class(IUnknown)
    function GetTypeInfoCount(var pctinfo: Integer): HResult; virtual; cdecl; export; abstract;
    function GetTypeInfo(itinfo: Integer; TLCID: TLCID; var pptinfo: ITypeInfo): HResult; virtual; cdecl; export; abstract;
    function GetIDsOfNames(const riid: IID; var rgszNames: PChar;
      cNames: Integer; TLCID: TLCID; rgdispid: PDispID): HResult; virtual; cdecl; export; abstract;
    function Invoke(dispidMember: TDispID; const riid: IID; TLCID: TLCID;
      wFlags: Word; var pdispparams: TDispParams; pvarResult: PVARIANT;
      var pexcepinfo: TExcepInfo; var puArgErr: Integer): HResult; virtual; cdecl; export; abstract;
  end;

function DispInvoke(_this: Pointer; ptinfo: ITypeInfo; dispidMember: TDispID;
  wFlags: Word; var pparams: TDispParams; pvarResult: PVARIANT;
  var pexcepinfo: TExcepInfo; var puArgErr: Integer): HResult; far; external 'ole2disp';
function DispGetIDsOfNames(ptinfo: ITypeInfo; var rgszNames: PChar;
  cNames: Integer; rgdispid: PDispID): HResult; far; external 'ole2disp';

function GUID_NULL: GUID;
begin
  Result := IID_NULL;
end;

{$ENDIF WIN32}

{ Standard OLE Library initialization code }

procedure InitOLE;
var
  dwVer: Longint;
  HRes: HResult;
begin
  if FOleInitialized then Exit;
  dwVer := Longint(CoBuildVersion);
  if (RMM <> HiWord(dwVer)) or (RUP > LoWord(dwVer)) then
    raise EOleError.Create(SOleInvalidVer)
  else begin
    HRes := OleInitialize(nil);
    if FailedHR(HRes) then
      raise EOleError.CreateFmt(SOleInitFailed, [Longint(HRes)])
    else FOleInitialized := True;
  end;
end;

{ Standard OLE Library exit code }

procedure DoneOLE;
begin
  if FOleInitialized then OleUninitialize;
  FOleInitialized := False;
end;

function OleInitialized: Boolean;
begin
  Result := FOleInitialized;
end;

procedure CheckOleInitialized;
begin
  if not FOleInitialized then raise EOleError.Create(SOleNotInit);
end;

{$IFNDEF COMPILER3_UP}
function OleErrorMsg(ErrorCode: HResult): string;
begin
  FmtStr(Result, SOleError, [Longint(ErrorCode)]);
end;
{$ENDIF}

{$IFNDEF WIN32}

procedure OleError(ErrorCode: HResult);
begin
  raise EOleError.Create(OleErrorMsg(ErrorCode));
end;

{ Raise EOleError exception if result code indicates an error }

procedure OleCheck(OleResult: HResult);
begin
  if FailedHR(OleResult) then OleError(OleResult);
end;

{$ENDIF WIN32}

{ Raise exception given an OLE return code and TExcepInfo structure }

procedure DispInvokeError(Status: HResult; const ExcepInfo: TExcepInfo);
{$IFDEF COMPILER3_UP}
begin
  DispatchInvokeError(Status, ExcepInfo);
{$ELSE}
var
  EClass: ExceptClass;
  Message: string;
begin
  EClass := EOleError;
  if Longint(Status) <> DISP_E_EXCEPTION then
    Message := OleErrorMsg(Status)
  else
    with ExcepInfo do
    begin
      try
        if (scode = CTL_E_SETNOTSUPPORTED) or
          (scode = CTL_E_SETNOTSUPPORTEDATRUNTIME) then
            EClass := EPropReadOnly
        else if (scode = CTL_E_GETNOTSUPPORTED) or
          (scode = CTL_E_GETNOTSUPPORTEDATRUNTIME) then
            EClass := EPropWriteOnly;
        if bstrDescription <> nil then begin
          Message := OleStrToString(bstrDescription);
          while (Length(Message) > 0) and
            (Message[Length(Message)] in [#0..#32, '.']) do
            Delete(Message, Length(Message), 1);
        end;
      finally
        if bstrSource <> nil then SysFreeString(bstrSource);
        if bstrDescription <> nil then SysFreeString(bstrDescription);
        if bstrHelpFile <> nil then SysFreeString(bstrHelpFile);
      end;
    end;
  if Message = '' then Message := OleErrorMsg(Status);
  raise EClass.Create(Message);
{$ENDIF COMPILER3_UP}
end;

{$IFNDEF WIN32}

{ Convert a string to a class ID }

function StringToClassID(const S: string): CLSID;
var
  CharBuf: array[0..64] of Char;
begin
  OleCheck(CLSIDFromString(StrPLCopy(CharBuf, S, SizeOf(CharBuf) - 1),
    Result));
end;

{ Convert a class ID to a string }

function ClassIDToString(const CLSID: CLSID): string;
var
  P: PChar;
  Malloc: IMalloc;
begin
  OleCheck(CoGetMalloc(MEMCTX_TASK, Malloc));
  OleCheck(StringFromCLSID(CLSID, P));
  Result := StrPas(P);
  Malloc.Free(P);
end;

{ Create an OLE object variant given an IDispatch }

function VarFromInterface(Unknown: IUnknown): Variant;
var
  Disp: IDispatch;
begin
  VariantClear(VARIANTARG(Result));
  VariantInit(VARIANTARG(Result));
  try
    if Unknown <> nil then begin
      OleCheck(Unknown.QueryInterface(IID_IDispatch, Disp));
      Result.VT := VT_DISPATCH;
      Result.pdispVal := Dispatch.IDispatch(Disp);
    end;
  except
    VariantClear(VARIANTARG(Result));
    raise;
  end;
end;

{ Return OLE object stored in a variant }

function VarToInterface(const V: Variant): IDispatch;
begin
  Result := nil;
  if V.VT = VT_DISPATCH then
    Result := IDispatch(V.pdispVal)
  else if V.VT = (VT_DISPATCH or VT_BYREF) then
    Result := IDispatch(V.ppdispVal^);
  if Result = nil then raise EOleError.Create(SOleNotReference);
end;

{ Create an OLE object variant given a class name }

function CreateOleObject(const ClassName: string): Variant;
var
  Unknown: IUnknown;
  ClassID: CLSID;
  CharBuf: array[0..127] of Char;
begin
  StrPLCopy(CharBuf, ClassName, SizeOf(CharBuf) - 1);
  OleCheck(CLSIDFromProgID(@CharBuf, ClassID));
  OleCheck(CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IID_IUnknown, Unknown));
  try
    Result := VarFromInterface(Unknown);
  finally
    Unknown.Release;
  end;
end;

{ Get active OLE object for a given class name }

function GetActiveOleObject(const ClassName: string): Variant;
var
  Unknown: IUnknown;
  ClassID: CLSID;
  CharBuf: array[0..127] of Char;
begin
  StrPLCopy(CharBuf, ClassName, SizeOf(CharBuf) - 1);
  OleCheck(CLSIDFromProgID(@CharBuf, ClassID));
  OleCheck(GetActiveObject(ClassID, nil, Unknown));
  try
    Result := VarFromInterface(Unknown);
  finally
    Unknown.Release;
  end;
end;

{ OLE string support }

function OleStrToString(Source: BSTR): string;
begin
  Result := StrPas(Source);
end;

function StringToOleStr(const Source: string): BSTR;
var
  SourceLen: Integer;
  CharBuf: array[0..255] of Char;
begin
  SourceLen := Length(Source);
  if SourceLen > 0 then begin
    StrPLCopy(CharBuf, Source, SizeOf(CharBuf) - 1);
    Result := SysAllocStringLen(CharBuf, SourceLen);
  end
  else Result := nil;
end;

{$ELSE}
 {$IFDEF COMPILER3_UP}

{ Return OLE object stored in a variant }

function VarToInterface(const V: Variant): IDispatch;
begin
  Result := nil;
  if TVarData(V).VType = varDispatch then
    Result := IDispatch(TVarData(V).VDispatch)
  else if TVarData(V).VType = (varDispatch or varByRef) then
    Result := IDispatch(Pointer(TVarData(V).VPointer^));
  if Result = nil then raise EOleError.Create(SOleNotReference);
end;

 {$ENDIF}
{$ENDIF}

{ Assign Variant }

procedure AssignVariant(
  var Dest: {$IFDEF WIN32} TVariantArg; {$ELSE} Variant; {$ENDIF}
  const Value: TVarRec);
begin
{$IFNDEF WIN32}
  VariantInit(VARIANTARG(Dest));
  try
{$ENDIF}
    with Value do
      case VType of
        vtInteger:
          begin
            Dest.vt := VT_I4;
            Dest.lVal := VInteger;
          end;
        vtBoolean:
          begin
            Dest.vt := VT_BOOL;
            Dest.vbool := VBoolean;
          end;
        vtChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(VChar);
          end;
        vtExtended:
          begin
            Dest.vt := VT_R8;
            Dest.dblVal := VExtended^;
          end;
        vtString:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(VString^);
          end;
        vtPointer:
          if VPointer = nil then begin
            Dest.vt := VT_NULL;
            Dest.byRef := nil;
          end
          else begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VPointer;
          end;
        vtPChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(StrPas(VPChar));
          end;
        vtObject:
          begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VObject;
          end;
{$IFDEF WIN32}
        vtClass:
          begin
            Dest.vt := VT_BYREF;
            Dest.byRef := VClass;
          end;
        vtWideChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := @VWideChar;
          end;
        vtPWideChar:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := VPWideChar;
          end;
        vtAnsiString:
          begin
            Dest.vt := VT_BSTR;
            Dest.bstrVal := StringToOleStr(string(VAnsiString));
          end;
        vtCurrency:
          begin
            Dest.vt := VT_CY;
            Dest.cyVal := VCurrency^;
          end;
        vtVariant:
          begin
            Dest.vt := VT_BYREF or VT_VARIANT;
            Dest.pvarVal := VVariant;
          end;
{$ENDIF WIN32}
{$IFDEF COMPILER4_UP}
        vtInterface:
          begin
            Dest.vt := VT_UNKNOWN or VT_BYREF;
            Dest.byRef := VInterface;
          end;
        vtInt64:
          begin
            Dest.vt := VT_I8 or VT_BYREF;
            Dest.byRef := VInt64;
          end;
{$ENDIF COMPILER4_UP}
        else raise EOleError.Create(SOleInvalidParam);
      end;
{$IFNDEF WIN32}
  except
    VariantClear(VARIANTARG(Dest));
    raise;
  end;
{$ENDIF}
end;

{ TJvOleController }

constructor TJvOleController.Create;
begin
  inherited Create;
{$IFDEF WIN32}
  FLocale := GetThreadLocale;
{$ELSE}
  FLocale := LOCALE_SYSTEM_DEFAULT;
{$ENDIF}
  try
    InitOLE;
  except
    Application.HandleException(Self);
  end;
end;

destructor TJvOleController.Destroy;
begin
  if FOleInitialized then ClearObject;
  inherited Destroy;
end;

procedure TJvOleController.CreateObject(const ClassName: string);
begin
  CheckOleInitialized;
  ClearObject;
  FObject := CreateOleObject(ClassName);
end;

procedure TJvOleController.GetActiveObject(const ClassName: string);
begin
  CheckOleInitialized;
  ClearObject;
  FObject := GetActiveOleObject(ClassName);
end;

procedure TJvOleController.AssignIDispatch(V: Variant);
begin
  CheckOleInitialized;
  ClearObject;
  VarToInterface(V);
{$IFDEF WIN32}
  VarCopy(FObject, V);
{$ELSE}
  VariantCopy(VARIANTARG(FObject), V);
{$ENDIF}
end;

procedure TJvOleController.ClearObject;
begin
{$IFDEF WIN32}
  VarClear(FRetValue);
  VarClear(FObject);
{$ELSE}
  VariantClear(VARIANTARG(FRetValue));
  VariantClear(VARIANTARG(FObject));
{$ENDIF}
end;

function TJvOleController.NameToDispID(const AName: string): TDispID;
var
{$IFDEF WIN32}
  CharBuf: array[0..255] of WideChar;
  P: array[0..0] of PWideChar;
{$ELSE}
  CharBuf: array[0..255] of Char;
  P: PChar;
{$ENDIF}
begin
  CheckOleInitialized;
{$IFDEF WIN32}
  StringToWideChar(AName, @CharBuf, 256);
  P[0] := @CharBuf[0];
{$ELSE}
  StrPLCopy(CharBuf, AName, SizeOf(CharBuf) - 1);
  P := @CharBuf;
{$ENDIF}
  if FailedHR(VarToInterface(FObject).GetIDsOfNames(GUID_NULL,
    {$IFDEF WIN32} @P, {$ELSE} P, {$ENDIF} 1, FLocale, @Result)) then
    raise EOleError.CreateFmt(SOleNotSupport, [AName]);
end;

function TJvOleController.NameToDispIDs(const AName: string;
  const AParams: array of string; Dest: PDispIDList): PDispIDList;
var
{$IFDEF WIN32}
  CharBuf: array[0..MaxDispArgs] of PWideChar;
  Size: Integer;
{$ELSE}
  CharBuf: array[0..MaxDispArgs] of PChar;
{$ENDIF}
  I: Byte;
begin
  Result := Dest;
  CheckOleInitialized;
{$IFDEF WIN32}
  Size := Length(AName) + 1;
  GetMem(CharBuf[0], Size * SizeOf(WideChar));
  StringToWideChar(AName, CharBuf[0], Size);
  for I := 0 to High(AParams) do begin
    Size := Length(AParams[I]) + 1;
    GetMem(CharBuf[I + 1], Size * SizeOf(WideChar));
    StringToWideChar(AParams[I], CharBuf[I + 1], Size);
  end;
{$ELSE}
  CharBuf[0] := StrPCopy(StrAlloc(Length(AName) + 1), AName);
  for I := 0 to High(AParams) do
    CharBuf[I + 1] := StrPCopy(StrAlloc(Length(AParams[I]) + 1), AParams[I]);
{$ENDIF}
  try
    if FailedHR(VarToInterface(FObject).GetIDsOfNames(GUID_NULL,
      {$IFDEF WIN32} @CharBuf, {$ELSE} CharBuf[0], {$ENDIF}
      High(AParams) + 2, FLocale, @Result^[0]))
    then
      raise EOleError.CreateFmt(SOleNotSupport, [AName]);
  finally
{$IFDEF WIN32}
    for I := 0 to High(AParams) + 1 do FreeMem(CharBuf[I]);
{$ELSE}
    for I := 0 to High(AParams) + 1 do StrDispose(CharBuf[I]);
{$ENDIF}
  end;
end;

function TJvOleController.Invoke(dispidMember: TDispID; wFlags: Word;
  var pdispparams: TDispParams; Res: PVariant): PVariant;
var
  pexcepinfo: TExcepInfo;
  puArgErr: Integer;
  HRes: HResult;
begin
{$IFDEF WIN32}
  if Res <> nil then VarClear(Res^);
  try
    HRes := VarToInterface(FObject).Invoke(dispidMember, GUID_NULL,
      FLocale, wFlags, pdispparams, Res, @pexcepinfo, @puArgErr);
  except
    if Res <> nil then VarClear(Res^);
    raise;
  end;
{$ELSE}
  if Res <> nil then begin
    VariantClear(VARIANTARG(Res^));
    VariantInit(VARIANTARG(Res^));
  end;
  try
    HRes := VarToInterface(FObject).Invoke(dispidMember, GUID_NULL,
      FLocale, wFlags, pdispparams, Res, pexcepinfo, puArgErr);
  except
    if Res <> nil then VariantClear(VARIANTARG(Res^));
    raise;
  end;
{$ENDIF}
  if FailedHR(HRes) then DispInvokeError(HRes, pexcepinfo);
  Result := Res;
end;

function TJvOleController.CallMethodNoParams(ID: TDispID;
  NeedResult: Boolean): PVariant;
const
  Disp: TDispParams = (rgvarg: nil; rgdispidNamedArgs: nil; cArgs: 0;
    cNamedArgs: 0);
begin
  CheckOleInitialized;
  if NeedResult then
    Result := Invoke(ID, DISPATCH_METHODNOPARAM, Disp, @FRetValue)
  else
    Result := Invoke(ID, DISPATCH_METHODNOPARAM, Disp, nil);
end;

function TJvOleController.CallMethod(ID: TDispID; const Params: array of const;
  NeedResult: Boolean): PVariant;
var
  Disp: TDispParams;
  ArgCnt, I: Integer;
{$IFDEF WIN32}
  Args: array[0..MaxDispArgs - 1] of TVariantArg;
{$ELSE}
  Args: array[0..MaxDispArgs - 1] of Variant;
{$ENDIF}
begin
  CheckOleInitialized;
  ArgCnt := 0;
  try
    for I := 0 to High(Params) do begin
      AssignVariant(Args[I], Params[I]);
      Inc(ArgCnt);
      if ArgCnt >= MaxDispArgs then Break;
    end;
    with Disp do begin
      if ArgCnt = 0 then rgvarg := nil
      else rgvarg := @Args;
      rgdispidNamedArgs := nil;
      cArgs := ArgCnt;
      cNamedArgs := 0;
    end;
    if NeedResult then
      Result := Invoke(ID, DISPATCH_METHODPARAMS, Disp, @FRetValue)
    else
      Result := Invoke(ID, DISPATCH_METHODPARAMS, Disp, nil);
  finally
{$IFNDEF WIN32}
    for I := 0 to ArgCnt - 1 do VariantClear(VARIANTARG(Args[I]));
{$ENDIF}
  end;
end;

function TJvOleController.CallMethodNamedParams(const IDs: TDispIDList;
  const Params: array of const; Cnt: Byte; NeedResult: Boolean): PVariant;
var
  Disp: TDispParams;
  ArgCnt, I: Integer;
{$IFDEF WIN32}
  Args: array[0..MaxDispArgs - 1] of TVariantArg;
{$ELSE}
  Args: array[0..MaxDispArgs - 1] of Variant;
{$ENDIF}
begin
  CheckOleInitialized;
  ArgCnt := 0;
  try
    for I := 0 to High(Params) do begin
      AssignVariant(Args[I], Params[I]);
      Inc(ArgCnt);
      if ArgCnt >= MaxDispArgs then Break;
    end;
    with Disp do begin
      if ArgCnt = 0 then rgvarg := nil
      else rgvarg := @Args;
      if Cnt = 0 then rgdispidNamedArgs := nil
      else rgdispidNamedArgs := @IDs[1];
      cArgs := ArgCnt;
      cNamedArgs := Cnt;
    end;
    if NeedResult then
      Result := Invoke(IDs[0], DISPATCH_METHODPARAMS, Disp, @FRetValue)
    else
      Result := Invoke(IDs[0], DISPATCH_METHODPARAMS, Disp, nil);
  finally
{$IFNDEF WIN32}
    for I := 0 to ArgCnt - 1 do VariantClear(VARIANTARG(Args[I]));
{$ENDIF}
  end;
end;

procedure TJvOleController.SetPropertyByID(ID: TDispID; const Prop: array of const);
const
  NameArg: TDispID = DISPID_PROPERTYPUT;
var
  Disp: TDispParams;
  ArgCnt, I: Integer;
{$IFDEF WIN32}
  Args: array[0..MaxDispArgs - 1] of TVariantArg;
{$ELSE}
  Args: array[0..MaxDispArgs - 1] of Variant;
{$ENDIF}
begin
  CheckOleInitialized;
  ArgCnt := 0;
  try
    for I := 0 to High(Prop) do begin
      AssignVariant(Args[I], Prop[I]);
      Inc(ArgCnt);
      if ArgCnt >= MaxDispArgs then Break;
    end;
    with Disp do begin
      rgvarg := @Args;
      rgdispidNamedArgs := @NameArg;
      cArgs := ArgCnt;
      cNamedArgs := 1;
    end;
    Invoke(ID, DISPATCH_PROPERTYPUT, Disp, nil);
  finally
{$IFNDEF WIN32}
    for I := 0 to ArgCnt - 1 do VariantClear(VARIANTARG(Args[I]));
{$ENDIF}
  end;
end;

function TJvOleController.GetPropertyByID(ID: TDispID): PVariant;
const
  Disp: TDispParams = (rgvarg: nil; rgdispidNamedArgs: nil;
    cArgs: 0; cNamedArgs: 0);
begin
  CheckOleInitialized;
  Result := Invoke(ID, DISPATCH_PROPERTYGET, Disp, @FRetValue);
end;

procedure TJvOleController.CallProcedureByID(ID: TDispID; const Params: array of const);
begin
  CallMethod(ID, Params, False);
end;

function TJvOleController.CallFunctionByID(ID: TDispID;
  const Params: array of const): PVariant;
begin
  Result := CallMethod(ID, Params, True);
end;

procedure TJvOleController.CallProcedureByIDsNamedParams(const IDs: TDispIDList;
  const Params: array of const; Cnt: Byte);
begin
  CallMethodNamedParams(IDs, Params, Cnt, False);
end;

function TJvOleController.CallFunctionByIDsNamedParams(const IDs: TDispIDList;
  const Params: array of const; Cnt: Byte): PVariant;
begin
  Result := CallMethodNamedParams(IDs, Params, Cnt, True);
end;

procedure TJvOleController.CallProcedureNoParamsByID(ID: TDispID);
begin
  CallMethodNoParams(ID, False);
end;

function TJvOleController.CallFunctionNoParamsByID(ID: TDispID): PVariant;
begin
  Result := CallMethodNoParams(ID, True);
end;

procedure TJvOleController.SetProperty(const AName: string;
  const Prop: array of const);
begin
  SetPropertyByID(NameToDispID(AName), Prop);
end;

function TJvOleController.GetProperty(const AName: string): PVariant;
begin
  Result := GetPropertyByID(NameToDispID(AName));
end;

procedure TJvOleController.CallProcedure(const AName: string;
  const Params: array of const);
begin
  CallProcedureByID(NameToDispID(AName), Params);
end;

function TJvOleController.CallFunction(const AName: string;
  const Params: array of const): PVariant;
begin
  Result := CallFunctionByID(NameToDispID(AName), Params);
end;

procedure TJvOleController.CallProcedureNamedParams(const AName: string;
  const Params: array of const; const ParamNames: array of string);
var
  DispIDs: array[0..MaxDispArgs] of TDispID;
begin
  CallProcedureByIDsNamedParams(NameToDispIDs(AName, ParamNames, @DispIDs)^,
    Params, High(ParamNames) + 1);
end;

function TJvOleController.CallFunctionNamedParams(const AName: string;
  const Params: array of const; const ParamNames: array of string): PVariant;
var
  DispIDs: array[0..MaxDispArgs] of TDispID;
begin
  Result := CallFunctionByIDsNamedParams(NameToDispIDs(AName, ParamNames,
    @DispIDs)^, Params, High(ParamNames) + 1);
end;

procedure TJvOleController.CallProcedureNoParams(const AName: string);
begin
  CallProcedureNoParamsByID(NameToDispID(AName));
end;

function TJvOleController.CallFunctionNoParams(const AName: string): PVariant;
begin
  Result := CallFunctionNoParamsByID(NameToDispID(AName));
end;

procedure TJvOleController.SetLocale(PrimaryLangID, SubLangID: Word);
begin
  FLocale := CreateLCID(PrimaryLangID, SubLangID);
end;

{ Utility routines }

function MakeLangID(PrimaryLangID, SubLangID: Word): Word;
begin
  Result := (SubLangID shl 10) or PrimaryLangID;
end;

function MakeLCID(LangID: Word): TLCID;
begin
  Result := TLCID(LangID or (Longint(0) shl 16));
end;

function CreateLCID(PrimaryLangID, SubLangID: Word): TLCID;
begin
  Result := MakeLCID(MakeLangID(PrimaryLangID, SubLangID));
end;

function ExtractLangID(LCID: TLCID): Word;
begin
  Result := LCID and $FF;
end;

function ExtractSubLangID(LCID: TLCID): Word;
begin
  Result := LCID and ($FF shl 10) shr 10;
end;

{$IFDEF WIN32}
initialization
finalization
  DoneOLE;
{$ELSE}
initialization
  AddExitProc(DoneOLE);
{$ENDIF}
end.
