{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimpleXML.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Christophe Paris.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues: This component does not parse the !DOCTYPE tags but preserves them
-----------------------------------------------------------------------------}
// $Id$

unit JvSimpleXml;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  IniFiles;

type
  {$IFDEF COMPILER5}
  THashedStringList = class(TStringList);
  THandle = Longword;
  {$ENDIF COMPILER5}
  TJvSimpleXML = class;
  EJvSimpleXMLError = class(Exception);
  TJvSimpleXMLElem = class;
  TJvSimpleXMLElems = class;
  TJvSimpleXMLProps = class;
  TJvSimpleXMLElemComment = class;
  TJvSimpleXMLElemClassic = class;
  TJvSimpleXMLElemCData = class;
  TJvSimpleXMLElemText = class;
  TJvSimpleXMLElemHeader = class;
  TJvOnSimpleXMLParsed = procedure(Sender: TObject; Name: string) of object;
  TJvOnValueParsed = procedure(Sender: TObject; Name, Value: string) of object;
  TJvOnSimpleProgress = procedure(Sender: TObject; const Position, Total: Integer) of object;

  //Those hash stuffs are for future use only
  //Plans are to replace current hash by this mechanism
  TJvHashKind = (hkList, hkDirect);
  PJvHashElem = ^TJvHashElem;
  TJvHashElem = packed record
    Next: PJvHashElem;
    Obj: TObject;
  end;
  PJvHashRecord = ^TJvHashRecord;
  TJvHashList = array [0..25] of PJvHashRecord;
  PJvHashList = ^TJvHashList;
  TJvHashRecord = packed record
    Count: Byte;
    case Kind: TJvHashKind of
      hkList: (List: PJvHashList);
      hkDirect: (FirstElem: PJvHashElem);
  end;

  TJvSimpleHashTable = class(TObject)
  private
    FList: PJvHashRecord;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(const AName: string; AObject: TObject);
    procedure Clear;
  end;

  TJvSimpleXMLProp = class(TObject)
  private
    FName: string;
    FValue: string;
    FParent: TJvSimpleXMLProps;
    FPointer: string;
    FData: Pointer;
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
  protected
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    function GetSimpleXML: TJvSimpleXML;
    function SaveToString: string;
    property Parent: TJvSimpleXMLProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property Pointer: string read FPointer write FPointer;

    property Data: Pointer read FData write FData;
  end;

  TJvSimpleXMLProps = class(TObject)
  private
    FProperties: THashedStringList;
    FParent: TJvSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXMLProp;
  protected
    function GetSimpleXML: TJvSimpleXML;
    function GetItem(const Index: Integer): TJvSimpleXMLProp;
    procedure DoItemRename(var Value: TJvSimpleXMLProp; const Name: string);
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(Parent: TJvSimpleXMLElem);
    destructor Destroy; override;
    function Add(const Name, Value: string): TJvSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXMLProp; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    property Item[const Index: Integer]: TJvSimpleXMLProp read GetItem; default;
    property ItemNamed[const Name: string]: TJvSimpleXMLProp read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXMLElemsProlog = class(TObject)
  private
    FElems: THashedStringList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TJvSimpleXMLElem;
    function GetEncoding: string;
    function GetStandAlone: Boolean;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetStandAlone(const Value: Boolean);
    procedure SetVersion(const Value: string);
  protected
    function FindHeader: TJvSimpleXMLElem;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil): string;
    procedure SaveToStream(const Stream: TStream; Parent: TJvSimpleXML = nil);
    property Item[const Index: Integer]: TJvSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
    property Encoding: string read GetEncoding write SetEncoding;
    property StandAlone: Boolean read GetStandAlone write SetStandAlone;
    property Version: string read GetVersion write SetVersion;
  end;

  TJvSimpleXMLElemCompare = function(Elems: TJvSimpleXMLElems; Index1, Index2: Integer): Integer of object;
  TJvSimpleXMLElems = class(TObject)
  private
    FParent: TJvSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXMLElem;
  protected
    FElems: THashedStringList;
    FCompare: TJvSimpleXMLElemCompare;
    function GetItem(const Index: Integer): TJvSimpleXMLElem;
    procedure AddChild(const Value: TJvSimpleXMLElem);
    procedure AddChildFirst(const Value: TJvSimpleXMLElem);
    procedure DoItemRename(var Value: TJvSimpleXMLElem; const Name: string);
    procedure CreateElems;
  public
    constructor Create(const AOwner: TJvSimpleXMLElem);
    destructor Destroy; override;

    // Use notify to indicate to a list that the given element is removed
    // from the list so that it doesn't delete it as well as the one
    // that insert it in itself. This method is automatically called
    // by AddChild and AddChildFirst if the Container property of the
    // given element is set.
    procedure Notify(Value: TJvSimpleXMLElem; Operation: TOperation);

    function Add(const Name: string): TJvSimpleXMLElemClassic; overload;
    function Add(const Name, Value: string): TJvSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: TStream): TJvSimpleXMLElemClassic; overload;
    function Add(Value: TJvSimpleXMLElem): TJvSimpleXMLElem; overload;
    function AddFirst(Value: TJvSimpleXMLElem): TJvSimpleXMLElem; overload;
    function AddFirst(const Name: string): TJvSimpleXMLElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TJvSimpleXMLElemComment;
    function AddCData(const Name: string; const Value: string): TJvSimpleXMLElemCData;
    function AddText(const Name: string; const Value: string): TJvSimpleXMLElemText;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure BinaryValue(const Name: string; const Stream: TStream);
    function LoadFromStream(const Stream: TStream; AParent: TJvSimpleXML = nil): string;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil);
    procedure Sort;
    procedure CustomSort(AFunction: TJvSimpleXMLElemCompare);
    property Parent: TJvSimpleXMLElem read FParent write FParent;
    property Item[const Index: Integer]: TJvSimpleXMLElem read GetItem; default;
    property ItemNamed[const Name: string]: TJvSimpleXMLElem read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXMLElem = class(TObject)
  private
    FName: string;
    FParent: TJvSimpleXMLElem;
    FItems: TJvSimpleXMLElems;
    FProps: TJvSimpleXMLProps;
    FValue: string;
    FPointer: string;
    FData: Pointer;
    FSimpleXML: TJvSimpleXML;
    FContainer: TJvSimpleXMLElems;
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
  protected
    function GetSimpleXML: TJvSimpleXML;
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TJvSimpleXMLProps;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
    function GetItems: TJvSimpleXMLElems;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(const AOwner: TJvSimpleXMLElem); virtual;
    destructor Destroy; override;
    procedure Assign(Value: TJvSimpleXMLElem);
    procedure Clear; virtual;
    function SaveToString: string;
    procedure LoadFromString(const Value: string);
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); virtual; abstract;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); virtual;
      abstract;
    procedure GetBinaryValue(const Stream: TStream);
    property Data: Pointer read FData write FData;
    function GetChildIndex(const AChild: TJvSimpleXMLElem): Integer;

    property SimpleXML: TJvSimpleXML read GetSimpleXML;
    property Container: TJvSimpleXMLElems read FContainer write FContainer;
  published
    property Name: string read FName write SetName;
    property Parent: TJvSimpleXMLElem read FParent write FParent;
    property Pointer: string read FPointer write FPointer;
    property ChildsCount: Integer read GetChildsCount;
    property Items: TJvSimpleXMLElems read GetItems;
    property Properties: TJvSimpleXMLProps read GetProps;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property Value: string read FValue write FValue;
  end;

  TJvSimpleXMLElemComment = class(TJvSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
  end;

  TJvSimpleXMLElemClassic = class(TJvSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
  end;

  TJvSimpleXMLElemCData = class(TJvSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
  end;

  TJvSimpleXMLElemText = class(TJvSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
  end;

  TJvSimpleXMLElemHeader = class(TJvSimpleXMLElem)
  private
    FStandalone: Boolean;
    FEncoding: string;
    FVersion: string;
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
    property Version: string read FVersion write FVersion;
    property StandAlone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;
    constructor Create(const AOwner: TJvSimpleXMLElem); override;
  end;

  TJvSimpleXMLElemDocType = class(TJvSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
  end;

  TJvSimpleXMLElemSheet = class(TJvSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
  end;

  TJvSimpleXMLOptions = set of (sxoAutoCreate, sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity);
  TJvSimpleXMLEncodeEvent = procedure(Sender: TObject; var Value: string) of object;
  TJvSimpleXMLEncodeStreamEvent = procedure(Sender: TObject; InStream, OutStream: TStream) of object;
  TJvSimpleXML = class(TComponent)
  protected
    FFileName: TFileName;
    FOptions: TJvSimpleXMLOptions;
    FRoot: TJvSimpleXMLElemClassic;
    FOnTagParsed: TJvOnSimpleXMLParsed;
    FOnValue: TJvOnValueParsed;
    FOnLoadProg: TJvOnSimpleProgress;
    FOnSaveProg: TJvOnSimpleProgress;
    FProlog: TJvSimpleXMLElemsProlog;
    FSaveCount: Integer;
    FSaveCurrent: Integer;
    FIndentString: string;
    FOnEncodeValue: TJvSimpleXMLEncodeEvent;
    FOnDecodeValue: TJvSimpleXMLEncodeEvent;
    FOnDecodeStream: TJvSimpleXMLEncodeStreamEvent;
    FOnEncodeStream: TJvSimpleXMLEncodeStreamEvent;
    procedure SetIndentString(const Value: string);
    procedure SetRoot(const Value: TJvSimpleXMLElemClassic);
    procedure SetFileName(Value: TFileName);
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
    procedure DoEncodeValue(var Value: string); virtual;
    procedure DoDecodeValue(var Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToStream(Stream: TStream);
    function SaveToString: string;
    property Prolog: TJvSimpleXMLElemsProlog read FProlog write FProlog;
    property Root: TJvSimpleXMLElemClassic read FRoot write SetRoot;
    property XMLData: string read SaveToString write LoadFromString;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property IndentString: string read FIndentString write SetIndentString;
    property Options: TJvSimpleXMLOptions read FOptions write FOptions default [sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
    property OnSaveProgress: TJvOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress: TJvOnSimpleProgress read FOnLoadProg write FOnLoadProg;
    property OnTagParsed: TJvOnSimpleXMLParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TJvOnValueParsed read FOnValue write FOnValue;
    property OnEncodeValue: TJvSimpleXMLEncodeEvent read FOnEncodeValue write FOnEncodeValue;
    property OnDecodeValue: TJvSimpleXMLEncodeEvent read FOnDecodeValue write FOnDecodeValue;
    property OnEncodeStream: TJvSimpleXMLEncodeStreamEvent read FOnEncodeStream write FOnEncodeStream;
    property OnDecodeStream: TJvSimpleXMLEncodeStreamEvent read FOnDecodeStream write FOnDecodeStream;
  end;

{$IFDEF COMPILER6_UP}

  TXMLVariant = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  TXMLVarData = packed record
    vType: TVarType;
    Reserved1: Word;
    Reserved2: Word;
    Reserved3: Word;
    XML: TJvSimpleXMLElem;
    Reserved4: Longint;
  end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJvSimpleXMLElem);
function XMLCreate(const AXML: TJvSimpleXMLElem): Variant; overload;
function XMLCreate: Variant; overload;
function VarXML: TVarType;

{$ENDIF COMPILER6_UP}

// Encodes a string into an internal format:
// any character <= #127 is preserved
// all other characters are converted to hex notation except
// for some special characters that are converted to XML entities
function SimpleXMLEncode(const S: string): string;
// Decodes a string encoded with SimpleXMLEncode:
// any character <= #127 is preserved
// all other characters and substrings are converted from
// the special XML entities to characters or from hex to characters
// NB! Setting TrimBlanks to true will slow down the process considerably
procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);

function XMLEncode(const S: string): string;
function XMLDecode(const S: string): string;

// Encodes special characters (', ", <, > and &) into XML entities (@apos;, &quot;, &lt;, &gt; and &amp;)
function EntityEncode(const S: string): string;
// Decodes XML entities (@apos;, &quot;, &lt;, &gt; and &amp;) into special characters (', ", <, > and &)
function EntityDecode(const S: string): string;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF COMPILER5}
  JvJCLUtils, // for StrToFloatDef
  {$ENDIF COMPILER6_UP}
  JvConsts, JvResources;

const
  cBufferSize = 8192;
  DefaultTrueBoolStr = 'True'; // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

var
  GlobalSorts: TList = nil;

  {$IFDEF COMPILER6_UP}
  GlobalXMLVariant: TXMLVariant = nil;
  {$ENDIF COMPILER6_UP}

  {$IFDEF COMPILER5}
  TrueBoolStrs: array of string;
  FalseBoolStrs: array of string;
  {$ENDIF COMPILER5}

function GSorts: TList;
begin
  if not Assigned(GlobalSorts) then
    GlobalSorts := TList.Create;
  Result := GlobalSorts;
end;

{$IFDEF COMPILER6_UP}
function XMLVariant: TXMLVariant;
begin
  if not Assigned(GlobalXMLVariant) then
    GlobalXMLVariant := TXMLVariant.Create;
  Result := GlobalXMLVariant;
end;
{$ENDIF COMPILER6_UP}

function EntityEncode(const S: string): string;
var
  I, J, K, L: Integer;
  tmp: string;
begin
  SetLength(Result, Length(S) * 6); // worst case
  J := 1;
  I := 1;
  L := Length(S);
  while I <= L do
  begin
    case S[I] of
      '"':
        tmp := '&quot;';
      '&':
        tmp := '&amp;';
      #39:
        tmp := '&apos;';
      '<':
        tmp := '&lt;';
      '>':
        tmp := '&gt;';
    else
      tmp := S[I];
    end;
    for K := 1 to Length(tmp) do
    begin
      Result[J] := tmp[K];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

function EntityDecode(const S: string): string;
var
  I, J, L: Integer;
begin
  Result := S;
  I := 1;
  J := 1;
  L := Length(Result);

  while I <= L do
  begin
    if Result[I] = '&' then
    begin
      if AnsiSameText(Copy(Result, I, 5), '&amp;') then
      begin
        Result[J] := '&';
        Inc(J);
        Inc(I, 4);
      end
      else
      if AnsiSameText(Copy(Result, I, 4), '&lt;') then
      begin
        Result[J] := '<';
        Inc(J);
        Inc(I, 3);
      end
      else
      if AnsiSameText(Copy(Result, I, 4), '&gt;') then
      begin
        Result[J] := '>';
        Inc(J);
        Inc(I, 3);
      end
      else
      if AnsiSameText(Copy(Result, I, 6), '&apos;') then
      begin
        Result[J] := #39;
        Inc(J);
        Inc(I, 5);
      end
      else
      if AnsiSameText(Copy(Result, I, 6), '&quot;') then
      begin
        Result[J] := '"';
        Inc(J);
        Inc(I, 5);
      end
      else
      begin
        Result[J] := Result[I];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := Result[I];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

{$IFDEF COMPILER5}

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

(*  make Delphi 5 compiler happy // andreas
procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const);
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;
*)

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
var
  lResult: Extended;

  function CompareWith(const AStrings: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(AStrings) to High(AStrings) do
      if AnsiSameText(S, AStrings[I]) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  Result := TryStrToFloat(S, lResult);
  if Result then
    Value := lResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

(*  make Delphi 5 compiler happy // andreas
function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    ConvertErrorFmt(@SInvalidBoolean, [S]);
end;
*)

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [Boolean] of string = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;

{$ENDIF COMPILER5}

function SimpleXMLEncode(const S: string): string;
const
  NoConversion = [#0..#127] - ['"', '&', #39, '<', '>'];
var
  I, J, K: Integer;
  tmp: string;
begin
  SetLength(Result, Length(S) * 6); // worst case
  J := 1;
  for I := 1 to Length(S) do
  begin
    if S[I] in NoConversion then
      Result[J] := S[I]
    else
    begin
      case S[I] of
        '"':
          tmp := '&quot;';
        '&':
          tmp := '&amp;';
        #39:
          tmp := '&apos;';
        '<':
          tmp := '&lt;';
        '>':
          tmp := '&gt;';
      else
        tmp := Format('&#x%.2x;', [Ord(S[I])]);
      end;
      for K := 1 to Length(tmp) do
      begin
        Result[J] := tmp[K];
        Inc(J);
      end;
      Dec(J);
    end;
    Inc(J);
  end;
  if J > 0 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);
var
  StringLength, ReadIndex, WriteIndex: Cardinal;

  procedure DecodeEntity(var S: string; StringLength: Cardinal;
    var ReadIndex, WriteIndex: Cardinal);
  const
    cHexPrefix: array [Boolean] of PChar = ('', '$');
  var
    I: Cardinal;
    Value: Integer;
    IsHex: Boolean;
  begin
    Inc(ReadIndex, 2);
    IsHex := (ReadIndex <= StringLength) and (S[ReadIndex] in ['x', 'X']);
    Inc(ReadIndex, Ord(IsHex));
    I := ReadIndex;
    while ReadIndex <= StringLength do
    begin
      if S[ReadIndex] = ';' then
      begin
        Value := StrToIntDef(cHexPrefix[IsHex] + Copy(S, I, ReadIndex - I), -1); // no characters are less than 0
        if Value > 0 then
          S[WriteIndex] := Chr(Value)
        else
          ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
        Exit;
      end;
      Inc(ReadIndex);
    end;
    ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
  end;

  procedure SkipBlanks(var S: string; StringLength: Cardinal; var ReadIndex: Cardinal);
  begin
    while ReadIndex < StringLength do
    begin
      if S[ReadIndex] = Cr then
        S[ReadIndex] := Lf
      else
      if S[ReadIndex + 1] = Cr then
        S[ReadIndex + 1] := Lf;
      if (S[ReadIndex] < #33) and (S[ReadIndex] = S[ReadIndex + 1]) then
        Inc(ReadIndex)
      else
        Exit;
    end;
  end;

begin
  // NB! This procedure replaces the text inplace to speed up the conversion. This
  // works because when decoding, the string can only become shorter. This is
  // accomplished by keeping track of the current read and write points.
  // In addition, the original string length is read only once and passed to the
  // inner procedures to speed up conversion as much as possible
  ReadIndex := 1;
  WriteIndex := 1;
  StringLength := Length(S);
  while ReadIndex <= StringLength do
  begin
    // this call lowers conversion speed by ~30%, ie 21MB/sec -> 15MB/sec (repeated tests, various inputs)
    if TrimBlanks then
      SkipBlanks(S, StringLength, ReadIndex);
    if S[ReadIndex] = '&' then
    begin
      if S[ReadIndex + 1] = '#' then
      begin
        DecodeEntity(S, StringLength, ReadIndex, WriteIndex);
        Inc(WriteIndex);
      end
      else
      if AnsiSameText(Copy(S, ReadIndex, 5), '&amp;') then
      begin
        S[WriteIndex] := '&';
        Inc(WriteIndex);
        Inc(ReadIndex, 4);
      end
      else
      if AnsiSameText(Copy(S, ReadIndex, 4), '&lt;') then
      begin
        S[WriteIndex] := '<';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if AnsiSameText(Copy(S, ReadIndex, 4), '&gt;') then
      begin
        S[WriteIndex] := '>';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if AnsiSameText(Copy(S, ReadIndex, 6), '&apos;') then
      begin
        S[WriteIndex] := #39;
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      if AnsiSameText(Copy(S, ReadIndex, 6), '&quot;') then
      begin
        S[WriteIndex] := '"';
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      begin
        S[WriteIndex] := S[ReadIndex];
        Inc(WriteIndex);
      end;
    end
    else
    begin
      S[WriteIndex] := S[ReadIndex];
      Inc(WriteIndex);
    end;
    Inc(ReadIndex);
  end;
  if WriteIndex > 0 then
    SetLength(S, WriteIndex - 1)
  else
    SetLength(S, 0);
    // this call lowers conversion speed by ~65%, ie 21MB/sec -> 7MB/sec (repeated tests, various inputs)
//  if TrimBlanks then
//    S := AdjustLineBreaks(S);
end;

function XMLEncode(const S: string): string;
begin
  Result := SimpleXMLEncode(S);
end;

function XMLDecode(const S: string): string;
begin
  Result := S;
  SimpleXMLDecode(Result, False);
end;

//=== { TJvSimpleXML } =======================================================

constructor TJvSimpleXML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoot := TJvSimpleXMLElemClassic.Create(nil);
  FRoot.FSimpleXML := Self;
  FProlog := TJvSimpleXMLElemsProlog.Create;
  FOptions := [sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
  FIndentString := '  ';
end;

destructor TJvSimpleXML.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited Destroy;
end;

procedure TJvSimpleXML.DoDecodeValue(var Value: string);
begin
  if Assigned(FOnDecodeValue) then
    FOnDecodeValue(Self, Value)
  else
  if sxoAutoEncodeValue in Options then
    SimpleXMLDecode(Value, False)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityDecode(Value);
end;

procedure TJvSimpleXML.DoEncodeValue(var Value: string);
begin
  if Assigned(FOnEncodeValue) then
    FOnEncodeValue(Self, Value)
  else
  if sxoAutoEncodeValue in Options then
    Value := SimpleXMLEncode(Value)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityEncode(Value);
end;

procedure TJvSimpleXML.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, APosition, ATotal);
end;

procedure TJvSimpleXML.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    Inc(FSaveCount);
    FOnSaveProg(Self, FSaveCurrent, FSaveCount);
  end;
end;

procedure TJvSimpleXML.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(Self, AName);
end;

procedure TJvSimpleXML.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(Self, AName, AValue);
end;

procedure TJvSimpleXML.LoadFromFile(const FileName: TFileName);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXML.LoadFromResourceName(Instance: THandle;
  const ResName: string);
const
  RT_RCDATA = PChar(10);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXML.LoadFromStream(Stream: TStream);
var
  AOutStream: TStream;
  DoFree: Boolean;
begin
  FRoot.Clear;
  FProlog.Clear;
  AOutStream := nil;
  DoFree := False;
  try
    if Assigned(FOnDecodeStream) then
    begin
      AOutStream := TMemoryStream.Create;
      DoFree := True;
      FOnDecodeStream(Self, Stream, AOutStream);
      AOutStream.Seek(0, soFromBeginning);
    end
    else
      AOutStream := Stream;
    if Assigned(FOnLoadProg) then
    begin
      FOnLoadProg(Self, AOutStream.Position, AOutStream.Size);
    // Read doctype and so on
      FProlog.LoadFromStream(AOutStream, Self);
    // Read elements
      FRoot.LoadFromStream(AOutStream, Self);
      FOnLoadProg(Self, AOutStream.Position, AOutStream.Size);
    end
    else
    begin
      if Assigned(FOnTagParsed) or Assigned(FOnValue) then
      begin
        FProlog.LoadFromStream(AOutStream, Self);
        FRoot.LoadFromStream(AOutStream, Self);
      end
      else
      begin
        FProlog.LoadFromStream(AOutStream);
        FRoot.LoadFromStream(AOutStream);
      end;
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

procedure TJvSimpleXML.LoadFromString(const Value: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXML.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  if FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenWrite);
    Stream.Size := 0;
  end
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXML.SaveToStream(Stream: TStream);
var
  lCount: Integer;
  AOutStream: TStream;
  DoFree: Boolean;
begin
  if Assigned(FOnEncodeStream) then
  begin
    AOutStream := TMemoryStream.Create;
    DoFree := True;
  end
  else
  begin
    AOutStream := Stream;
    DoFree := False;
  end;
  try
    if Assigned(FOnSaveProg) then
    begin
      lCount := Root.ChildsCount + Prolog.Count;
      FSaveCount := lCount;
      FSaveCurrent := 0;
      FOnSaveProg(Self, 0, lCount);
      Prolog.SaveToStream(AOutStream, Self);
      Root.SaveToStream(AOutStream, '', Self);
      FOnSaveProg(Self, lCount, lCount);
    end
    else
    begin
      Prolog.SaveToStream(AOutStream);
      Root.SaveToStream(AOutStream);
    end;
    if Assigned(FOnEncodeStream) then
    begin
      AOutStream.Seek(0, soFromBeginning);
      FOnEncodeStream(Self, AOutStream, Stream);
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

function TJvSimpleXML.SaveToString: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXML.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== { TJvSimpleXMLElem } ===================================================

procedure TJvSimpleXMLElem.Assign(Value: TJvSimpleXMLElem);
var
  Elems: TJvSimpleXMLElem;
  Elem: TJvSimpleXMLElem;
  I: Integer;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TJvSimpleXMLElem(Value);
  Name := Elems.Name;
  Self.Value := Elems.Value;
  for I := 0 to Elems.Properties.Count - 1 do
    Properties.Add(Elems.Properties[I].Name, Elems.Properties[I].Value);

  for I := 0 to Elems.Items.Count - 1 do
  begin
    Elem := Items.Add(Elems.Items[I].Name, Elems.Items[I].Value);
    Elem.Assign(TJvSimpleXMLElem(Elems.Items[I]));
  end;
end;

procedure TJvSimpleXMLElem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
  if FProps <> nil then
    FProps.Clear;
end;

constructor TJvSimpleXMLElem.Create(const AOwner: TJvSimpleXMLElem);
begin
  inherited Create;
  FName := '';
  FParent := TJvSimpleXMLElem(AOwner);
  FContainer := nil;
end;

destructor TJvSimpleXMLElem.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FProps);
  inherited Destroy;
end;

procedure TJvSimpleXMLElem.Error(const S: string);
begin
  raise EJvSimpleXMLError.Create(S);
end;

procedure TJvSimpleXMLElem.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

procedure TJvSimpleXMLElem.GetBinaryValue(const Stream: TStream);
var
  I, J: Integer;
  St: string;
  Buf: array [0..cBufferSize - 1] of Byte;
begin
  I := 1;
  J := 0;
  while I < Length(Value) do
  begin
    St := '$' + Value[I] + Value[I + 1];
    if J = cBufferSize - 1 then //Buffered write to speed up the process a little
    begin
      Stream.Write(Buf, J);
      J := 0;
    end;
    Buf[J] := StrToIntDef(St, 0);
    Inc(J);
    Inc(I, 2);
  end;
  Stream.Write(Buf, J);
end;

function TJvSimpleXMLElem.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJvSimpleXMLElem.GetChildIndex(
  const AChild: TJvSimpleXMLElem): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.FElems.IndexOfObject(AChild);
end;

function TJvSimpleXMLElem.GetChildsCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if FItems <> nil then
    for I := 0 to FItems.Count - 1 do
      Result := Result + FItems[I].ChildsCount;
end;

function TJvSimpleXMLElem.GetFloatValue: Extended;
begin
  Result := StrToFloatDef(Value, 0.0);
end;

function TJvSimpleXMLElem.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJvSimpleXMLElem.GetItems: TJvSimpleXMLElems;
begin
  if FItems = nil then
    FItems := TJvSimpleXMLElems.Create(Self);
  Result := FItems;
end;

function TJvSimpleXMLElem.GetProps: TJvSimpleXMLProps;
begin
  if FProps = nil then
    FProps := TJvSimpleXMLProps.Create(Self);
  Result := FProps;
end;

function TJvSimpleXMLElem.GetSimpleXML: TJvSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := FSimpleXML;
end;

procedure TJvSimpleXMLElem.LoadFromString(const Value: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TJvSimpleXMLElem.SaveToString: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TJvSimpleXMLElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJvSimpleXMLElem.SetFloatValue(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TJvSimpleXMLElem.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJvSimpleXMLElem.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.Items.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TJvSimpleXMLElems } ==================================================

function TJvSimpleXMLElems.Add(const Name: string): TJvSimpleXMLElemClassic;
begin
  Result := TJvSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChild(Result);
end;

function TJvSimpleXMLElems.Add(const Name, Value: string): TJvSimpleXMLElemClassic;
begin
  Result := TJvSimpleXMLElemClassic.Create(Parent);
  Result.Name := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXMLElems.Add(const Name: string; const Value: Int64): TJvSimpleXMLElemClassic;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJvSimpleXMLElems.Add(Value: TJvSimpleXMLElem): TJvSimpleXMLElem;
begin
  if Value <> nil then
    AddChild(Value);
  Result := Value;
end;

function TJvSimpleXMLElems.Add(const Name: string;
  const Value: Boolean): TJvSimpleXMLElemClassic;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TJvSimpleXMLElems.Add(const Name: string;
  const Value: TStream): TJvSimpleXMLElemClassic;
var
  Stream: TStringStream;
  Buf: array [0..cBufferSize - 1] of Byte;
  St: string;
  I, Count: Integer;
begin
  Stream := TStringStream.Create('');
  repeat
    Count := Value.Read(Buf, SizeOf(Buf));
    St := '';
    for I := 0 to Count - 1 do
      St := St + IntToHex(Buf[I], 2);
    Stream.WriteString(St);
  until Count = 0;
  Result := Add(Name, Stream.DataString);
  Stream.Free;
end;

procedure TJvSimpleXMLElems.AddChild(const Value: TJvSimpleXMLElem);
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
    Value.Container.Notify(Value, opRemove);

  FElems.AddObject(Value.Name, Value);

  Notify(Value, opInsert);
end;

procedure TJvSimpleXMLElems.AddChildFirst(const Value: TJvSimpleXMLElem);
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
    Value.Container.Notify(Value, opRemove);

  FElems.InsertObject(0, Value.Name, Value);

  Notify(Value, opInsert);
end;

function TJvSimpleXMLElems.AddFirst(const Name: string): TJvSimpleXMLElemClassic;
begin
  Result := TJvSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChildFirst(Result);
end;

function TJvSimpleXMLElems.AddFirst(Value: TJvSimpleXMLElem): TJvSimpleXMLElem;
begin
  if Value <> nil then
    AddChildFirst(Value);
  Result := Value;
end;

function TJvSimpleXMLElems.AddComment(const Name,
  Value: string): TJvSimpleXMLElemComment;
begin
  Result := TJvSimpleXMLElemComment.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXMLElems.AddCData(const Name, Value: string): TJvSimpleXMLElemCData;
begin
  Result := TJvSimpleXMLElemCData.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXMLElems.AddText(const Name, Value: string): TJvSimpleXMLElemText;
begin
  Result := TJvSimpleXMLElemText.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

procedure TJvSimpleXMLElems.BinaryValue(const Name: string;
  const Stream: TStream);
var
  Elem: TJvSimpleXMLElem;
begin
  Elem := GetItemNamed(Name);
  if Elem <> nil then
    Elem.GetBinaryValue(Stream);
end;

function TJvSimpleXMLElems.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  Elem: TJvSimpleXMLElem;
begin
  try
    Elem := GetItemNamed(Name);
    if (Elem = nil) or (Elem.Value = '') then
      Result := Default
    else
      Result := Elem.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJvSimpleXMLElems.Clear;
var
  I: Integer;
begin
  if FElems <> nil then
  begin
    for I := 0 to FElems.Count - 1 do
    begin
      // TJvSimpleXMLElem(FElems.Objects[I]).Clear; // (p3) not needed -called in Destroy
      FElems.Objects[I].Free;
      FElems.Objects[I] := nil;
    end;
    FElems.Clear;
  end;
end;

constructor TJvSimpleXMLElems.Create(const AOwner: TJvSimpleXMLElem);
begin
  inherited Create;
  FParent := AOwner;
end;

procedure TJvSimpleXMLElems.Delete(const Index: Integer);
begin
  if (FElems <> nil) and (Index >= 0) and (Index < FElems.Count) then
  begin
    TObject(FElems.Objects[Index]).Free;
    FElems.Delete(Index);
  end;
end;

procedure TJvSimpleXMLElems.CreateElems;
begin
  if FElems = nil then
    FElems := THashedStringList.Create;
end;

procedure TJvSimpleXMLElems.Delete(const Name: string);
begin
  if FElems <> nil then
    Delete(FElems.IndexOf(Name));
end;

destructor TJvSimpleXMLElems.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TJvSimpleXMLElems.DoItemRename(var Value: TJvSimpleXMLElem;
  const Name: string);
var
  I: Integer;
begin
  I := FElems.IndexOfObject(Value);
  if I <> -1 then
    FElems[I] := Name;
end;

function TJvSimpleXMLElems.GetCount: Integer;
begin
  if FElems = nil then
    Result := 0
  else
    Result := FElems.Count;
end;

function TJvSimpleXMLElems.GetItem(const Index: Integer): TJvSimpleXMLElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    Result := nil
  else
    Result := TJvSimpleXMLElem(FElems.Objects[Index]);
end;

function TJvSimpleXMLElems.GetItemNamed(const Name: string): TJvSimpleXMLElem;
var
  I: Integer;
begin
  Result := nil;
  if FElems <> nil then
  begin
    I := FElems.IndexOf(Name);
    if I <> -1 then
      Result := TJvSimpleXMLElem(FElems.Objects[I])
    else
    if Assigned(Parent) and
      Assigned(Parent.SimpleXML) and
      (sxoAutoCreate in Parent.SimpleXML.Options) then
      Result := Add(Name);
  end
  else
  if Assigned(Parent) and
    Assigned(Parent.SimpleXML) and
    (sxoAutoCreate in Parent.SimpleXML.Options) then
    Result := Add(Name);
end;

function TJvSimpleXMLElems.IntValue(const Name: string; Default: Int64): Int64;
var
  Elem: TJvSimpleXMLElem;
begin
  Elem := GetItemNamed(Name);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.IntValue;
end;

function TJvSimpleXMLElems.LoadFromStream(const Stream: TStream; AParent: TJvSimpleXML): string;
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lElem: TJvSimpleXMLElem;
begin
  lStreamPos := Stream.Position;
  Result := '';
  St := '';
  lPos := 0;

  // We read from a stream, thus replacing the existing items
  Clear;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces
          begin
            case lBuf[I] of
              ' ', Tab, Cr, Lf:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  St := lBuf[I];
                end;
            else
              begin
                  //This is a text
                lElem := TJvSimpleXMLElemText.Create(Parent);
                Stream.Seek(lStreamPos - 1, soFromBeginning);
                lElem.LoadFromStream(Stream);
                lStreamPos := Stream.Position;
                CreateElems;
                FElems.AddObject(lElem.Name, lElem);
                Break;
              end;
            end;
          end;

        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            case lBuf[I] of
              '/':
                if St = '<' then
                begin
                  lPos := 2;
                  St := '';
                end
                else
                begin
                  lElem := TJvSimpleXMLElemClassic.Create(Parent);
                  St := St + lBuf[I];
                end;

              ' ', '>', ':': //This should be a classic tag
                begin
                  lElem := TJvSimpleXMLElemClassic.Create(Parent);
                  St := St + lBuf[I];
                end;
            else
              begin
                St := St + lBuf[I];
                if St = '<![CDATA[' then
                  lElem := TJvSimpleXMLElemCData.Create(Parent)
                else
                if St = '<!--' then
                  lElem := TJvSimpleXMLElemComment.Create(Parent);
                  //<?
              end;
            end;

            if lElem <> nil then
            begin
              CreateElems;
              Stream.Seek(lStreamPos - (Length(St)), soFromBeginning);
              lElem.LoadFromStream(Stream);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              St := '';
              lPos := 0;
              Break;
            end;
          end;

        2: //This is an end tag
          if lBuf[I] = '>' then
          begin
            Result := St;
            Count := 0;
            Break;
          end
          else
            St := St + lBuf[I];
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElems.Notify(Value: TJvSimpleXMLElem;
  Operation: TOperation);
begin
  case Operation of
    opRemove:
      if Value.Container = Self then  // Only remove if we have it
        FElems.Delete(FElems.IndexOf(Value.Name));
    opInsert:
      Value.Container := Self;
  end;
end;

procedure TJvSimpleXMLElems.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXML);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, Level, Parent);
end;

function TJvSimpleXMLElems.Value(const Name: string; Default: string): string;
var
  Elem: TJvSimpleXMLElem;
begin
  Result := '';
  Elem := GetItemNamed(Name);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.Value;
end;

function SortItems(List: TStringList; Index1, Index2: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GSorts.Count - 1 do
    if TJvSimpleXMLElems(GSorts[I]).FElems = List then
    begin
      Result := TJvSimpleXMLElems(GSorts[I]).FCompare(TJvSimpleXMLElems(GSorts[I]), Index1, Index2);
      Break;
    end;
end;

procedure TJvSimpleXMLElems.CustomSort(AFunction: TJvSimpleXMLElemCompare);
begin
  if FElems <> nil then
  begin
    GSorts.Add(Self);
    FCompare := AFunction;
    FElems.CustomSort(SortItems);
    GSorts.Remove(Self);
  end;
end;

procedure TJvSimpleXMLElems.Sort;
begin
  if FElems <> nil then
    FElems.Sort;
end;

//=== { TJvSimpleXMLProps } ==================================================

function TJvSimpleXMLProps.Add(const Name, Value: string): TJvSimpleXMLProp;
var
  Elem: TJvSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TJvSimpleXMLProp.Create();
  FProperties.AddObject(Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TJvSimpleXMLProps.Add(const Name: string; const Value: Int64): TJvSimpleXMLProp;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJvSimpleXMLProps.Add(const Name: string; const Value: Boolean): TJvSimpleXMLProp;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TJvSimpleXMLProps.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  Prop: TJvSimpleXMLProp;
begin
  try
    Prop := GetItemNamed(Name);
    if (Prop = nil) or (Prop.Value = '') then
      Result := Default
    else
      Result := Prop.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJvSimpleXMLProps.Clear;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    for I := 0 to FProperties.Count - 1 do
    begin
      TJvSimpleXMLProp(FProperties.Objects[I]).Free;
      FProperties.Objects[I] := nil;
    end;
    FProperties.Clear;
  end;
end;

procedure TJvSimpleXMLProps.Delete(const Index: Integer);
begin
  if (FProperties <> nil) and (Index >= 0) and (Index < FProperties.Count) then
  begin
    TObject(FProperties.Objects[Index]).Free;
    FProperties.Delete(Index);
  end;
end;

constructor TJvSimpleXMLProps.Create(Parent: TJvSimpleXMLElem);
begin
  inherited Create;
  FParent := Parent;
end;

procedure TJvSimpleXMLProps.Delete(const Name: string);
begin
  if FProperties <> nil then
    Delete(FProperties.IndexOf(Name));
end;

destructor TJvSimpleXMLProps.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TJvSimpleXMLProps.DoItemRename(var Value: TJvSimpleXMLProp;
  const Name: string);
var
  I: Integer;
begin
  if FProperties = nil then
    Exit;
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

procedure TJvSimpleXMLProps.Error(const S: string);
begin
  raise EJvSimpleXMLError.Create(S);
end;

procedure TJvSimpleXMLProps.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TJvSimpleXMLProps.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

function TJvSimpleXMLProps.GetItem(const Index: Integer): TJvSimpleXMLProp;
begin
  if FProperties <> nil then
    Result := TJvSimpleXMLProp(FProperties.Objects[Index])
  else
    Result := nil;
end;

function TJvSimpleXMLProps.GetItemNamed(const Name: string): TJvSimpleXMLProp;
var
  I: Integer;
begin
  Result := nil;
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(Name);
    if I <> -1 then
      Result := TJvSimpleXMLProp(FProperties.Objects[I])
    else
    if Assigned(FParent) and
      Assigned(FParent.SimpleXML) and
      (sxoAutoCreate in FParent.SimpleXML.Options) then
      Result := Add(Name, '');
  end
  else
  if Assigned(FParent) and
    Assigned(FParent.SimpleXML) and
    (sxoAutoCreate in FParent.SimpleXML.Options) then
  begin
    Result := Add(Name, '');
  end;
end;

function TJvSimpleXMLProps.GetSimpleXML: TJvSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := nil;
end;

function TJvSimpleXMLProps.IntValue(const Name: string; Default: Int64): Int64;
var
  Prop: TJvSimpleXMLProp;
begin
  Prop := GetItemNamed(Name);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.IntValue;
end;

procedure TJvSimpleXMLProps.LoadFromStream(const Stream: TStream);
//<element Prop="foo" Prop='bar' foo:bar="beuh"/>
//Stop on / or ? or >
type
  TPosType = (
    ptWaiting,
    ptReadingName,
    ptStartingContent,
    ptReadingValue,
    ptSpaceBeforeEqual
    );
var
  lPos: TPosType;
  I, lStreamPos, Count: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lName, lValue, lPointer: string;
  lPropStart: Char;
begin
  lStreamPos := Stream.Position;
  lValue := '';
  lPointer := '';
  lName := '';
  lPropStart := ' ';
  lPos := ptWaiting;

  // We read from a stream, thus replacing the existing properties
  Clear;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        ptWaiting: //We are waiting for a property
          begin
            case lBuf[I] of
              ' ', Tab, Cr, Lf:
                begin
                end;
              'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
                begin
                  lName := lBuf[I];
                  lPointer := '';
                  lPos := ptReadingName;
                end;
              '/', '>', '?':
                begin
                  Dec(lStreamPos);
                  Count := 0;
                  Break;
                end;
            else
              FmtError(RsEInvalidXMLElementUnexpectedCharacte, [lBuf[I]]);
            end;
          end;

        ptReadingName: //We are reading a property name
          case lBuf[I] of
            'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
              lName := lName + lBuf[I];
            ':':
              begin
                lPointer := lName;
                lName := '';
              end;
            '=':
              lPos := ptStartingContent;
            ' ', Tab, Cr, Lf:
              lPos := ptSpaceBeforeEqual;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte, [lBuf[I]]);
          end;

        ptStartingContent: //We are going to start a property content
          case lBuf[I] of
            ' ', Tab, Cr, Lf:
              ; // ignore white space
            '''', '"':
              begin
                lPropStart := lBuf[I];
                lValue := '';
                lPos := ptReadingValue;
              end;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte_, [lBuf[I]]);
          end;
        ptReadingValue: //We are reading a property
          if lBuf[I] = lPropStart then
          begin
            if (GetSimpleXML <> nil) then
              GetSimpleXML.DoDecodeValue(lValue);
            with Add(lName, lValue) do
              Pointer := lPointer;
            lPos := ptWaiting;
          end
          else
            lValue := lValue + lBuf[I];
        ptSpaceBeforeEqual: // We are reading the white space between a property name and the = sign
          case lBuf[I] of
            ' ', Tab, Cr, Lf:
              ; // more white space, stay in this state and ignore
            '=':
              lPos := ptStartingContent;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte, [lBuf[I]]);
          end;
      else
        Assert(False, RsEUnexpectedValueForLPos);
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLProps.SaveToStream(const Stream: TStream);
var
  St: string;
  I: Integer;
begin
  St := '';
  for I := 0 to Count - 1 do
    St := St + Item[I].SaveToString;
  if St <> '' then
    Stream.Write(St[1], Length(St));
end;

function TJvSimpleXMLProps.Value(const Name: string; Default: string): string;
var
  Prop: TJvSimpleXMLProp;
begin
  Result := '';
  Prop := GetItemNamed(Name);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.Value;
end;

//=== { TJvSimpleXMLProp } ===================================================

function TJvSimpleXMLProp.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJvSimpleXMLProp.GetFloatValue: Extended;
begin
  Result := StrToFloatDef(Value, 0.0);
end;

function TJvSimpleXMLProp.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJvSimpleXMLProp.GetSimpleXML: TJvSimpleXML;
begin
  if (FParent <> nil) and (FParent.FParent <> nil) then
    Result := FParent.FParent.GetSimpleXML
  else
    Result := nil;
end;

function TJvSimpleXMLProp.SaveToString: string;
var
  AEncoder: TJvSimpleXML;
begin
  AEncoder := GetSimpleXML;
  if Pointer <> '' then
  begin
    if AEncoder <> nil then
      AEncoder.DoEncodeValue(FValue);
    Result := Format(' %s:%s="%s"', [Pointer, Name, FValue]);
  end
  else
  begin
    if AEncoder <> nil then
      AEncoder.DoEncodeValue(FValue);
    Result := Format(' %s="%s"', [Name, FValue]);
  end;
end;

procedure TJvSimpleXMLProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJvSimpleXMLProp.SetFloatValue(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TJvSimpleXMLProp.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJvSimpleXMLProp.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TJvSimpleXMLElemClassic } ============================================

procedure TJvSimpleXMLElemClassic.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St, lName, lValue, lPointer: string;
begin
  lStreamPos := Stream.Position;
  St := '';
  lValue := '';
  lPointer := '';
  lPos := 1;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1:
          if lBuf[I] = '<' then
            lPos := 2
          else
            FmtError(RsEInvalidXMLElementExpectedBeginningO, [lBuf[I]]);
        -1:
          if lBuf[I] = '>' then
          begin
            Count := 0;
            Break;
          end
          else
            FmtError(RsEInvalidXMLElementExpectedEndOfTagBu, [lBuf[I]]);
      else
        begin
          if lBuf[I] in [Tab, Lf, Cr, ' ' {, '.'}] then
          begin
            if lPos = 2 then
              Error(RsEInvalidXMLElementMalformedTagFoundn);
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Break; //Re read buffer
          end
          else
          begin
            case lBuf[I] of
              '>':
                begin
                  lName := St;
                  //Load elements
                  Stream.Seek(lStreamPos, soFromBeginning);
                  St := Items.LoadFromStream(Stream, Parent);
                  if lName <> St then
                    FmtError(RsEInvalidXMLElementErroneousEndOfTagE, [lName, St]);
                  lStreamPos := Stream.Position;

                  //Set value if only one sub element
                  //This might reduce speed, but this is for compatibility issues
                  if (Items.Count = 1) and (Items[0] is TJvSimpleXMLElemText) then
                  begin
                    lValue := Items[0].Value;
                    Items.Clear;
                  end;

                  Count := 0;
                  Break;
                end;
              '/':
                begin
                  lName := St;
                  lPos := -1;
                end;
              ':':
                begin
                  lPointer := St;
                  St := '';
                end;
            else
              begin
                St := St + lBuf[I];
                Inc(lPos);
              end;
            end;
          end;
        end;
      end;
    end;
  until Count = 0;

  Name := lName;
  if GetSimpleXML <> nil then
    GetSimpleXML.DoDecodeValue(lValue);
  Value := lValue;
  Pointer := lPointer;

  if Parent <> nil then
  begin
    Parent.DoTagParsed(lName);
    Parent.DoValueParsed(lName, lValue);
  end;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemClassic.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXML);
var
  St, AName: string;
  LevelAdd: string;
begin
  AName := Name;
  if Name <> '' then
  begin
    if GetSimpleXML <> nil then
       GetSimpleXML.DoEncodeValue(AName);
    St := Level + '<' + AName;

    Stream.Write(St[1], Length(St));
    Properties.SaveToStream(Stream);
  end;

  if (Items.Count = 0) then
  begin
    if (Name <> '') then
    begin
      if Value = '' then
        St := '/>' + sLineBreak
      else
      begin
        if GetSimpleXML <> nil then
          GetSimpleXML.DoEncodeValue(FValue);
        St := '>' + FValue + '</' + AName + '>' + sLineBreak;
      end;
      Stream.Write(St[1], Length(St));
    end;
  end
  else
  begin
    if (Name <> '') then
    begin
      St := '>' + sLineBreak;
      Stream.Write(St[1], Length(St));
    end;
    if Assigned(SimpleXML) and
      (sxoAutoIndent in SimpleXML.Options) then
    begin
      LevelAdd := SimpleXML.IndentString;
    end;
    Items.SaveToStream(Stream, Level + LevelAdd, Parent);
    if Name <> '' then
    begin
      St := Level + '</' + AName + '>' + sLineBreak;
      Stream.Write(St[1], Length(St));
    end;
  end;
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== { TJvSimpleXMLElemComment } ============================================

procedure TJvSimpleXMLElemComment.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
//<!-- declarations for <head> & <body> -->
const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT = '    -->';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<!--
          if lBuf[I] = CS_START_COMMENT[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidCommentExpectedsButFounds, [CS_START_COMMENT[lPos], lBuf[I]]);
        5:
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        6: //-
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
          begin
            St := St + '-' + lBuf[I];
            Dec(lPos);
          end;
        7: //>
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            if lBuf[I + 1] <> '>' then
              Error(RsEInvalidCommentNotAllowedInsideComme);
            St := St + '--' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemComment.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXML);
var
  St: string;
begin
  St := Level + '<!--';
  Stream.Write(St[1], Length(St));
  if Value <> '' then
    Stream.Write(Value[1], Length(Value));
  St := '-->' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== { TJvSimpleXMLElemCData } ==============================================

procedure TJvSimpleXMLElemCData.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA = '         ]]>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<![CDATA[
          if lBuf[I] = CS_START_CDATA[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidCDATAExpectedsButFounds, [CS_START_CDATA[lPos], lBuf[I]]);
        10:
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        11: //-
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
          begin
            St := St + ']' + lBuf[I];
            Dec(lPos);
          end;
        12: //>
          if lBuf[I] = CS_STOP_CDATA[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            St := St + ']]' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCDATAUnexpectedEndOfData);

  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemCData.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXML);
var
  St: string;
begin
  St := Level + '<![CDATA[';
  Stream.Write(St[1], Length(St));
  if Value <> '' then
    Stream.Write(Value[1], Length(Value));
  St := ']]>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== { TJvSimpleXMLElemText } ===============================================

procedure TJvSimpleXMLElemText.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 0;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lBuf[I] of
        '<':
          begin
            //Quit text
            Dec(lStreamPos);
            Count := 0;
            Break;
          end;
        ' ':
          if lPos = 0 then
          begin
            Inc(lPos);
            St := St + ' ';
          end;
      else
        begin
          lPos := 0;
          St := St + lBuf[I];
        end;
      end;
    end;
  until Count = 0;
  if GetSimpleXML <> nil then
    GetSimpleXML.DoDecodeValue(St);
  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemText.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXML);
var
  St: string;
begin
  if Value <> '' then
  begin
    if GetSimpleXML <> nil then
      GetSimpleXML.DoEncodeValue(FValue);
    St := Level + Value + sLineBreak;
    Stream.Write(St[1], Length(St));
  end;
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== { TJvSimpleXMLElemHeader } =============================================

constructor TJvSimpleXMLElemHeader.Create(const AOwner: TJvSimpleXMLElem);
begin
  inherited Create(AOwner);
  FVersion := '1.0';
  FEncoding := 'iso-8859-1';
  FStandalone := False;
end;

procedure TJvSimpleXMLElemHeader.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
//<?xml version="1.0" encoding="iso-xyzxx" standalone="yes"?>
const
  CS_START_HEADER = '<?xml';
  CS_STOP_HEADER = '     ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<?xml
          if lBuf[I] = CS_START_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_HEADER[lPos], lBuf[I]]);
        5: //L
          if lBuf[I] = CS_START_HEADER[lPos] then
          begin
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);

            FVersion := Properties.Value('version');
            FEncoding := Properties.Value('encoding');
            FStandalone := Properties.Value('standalone') = 'yes';

            Properties.Clear;

            Break; //Re read buffer
          end
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_HEADER[lPos], lBuf[I]]);
        6: //?
          if lBuf[I] = CS_STOP_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_STOP_HEADER[lPos], lBuf[I]]);
        7: //>
          if lBuf[I] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_STOP_HEADER[lPos], lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemHeader.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXML);
var
  St: string;
begin
  St := Level + '<?xml version="' + FVersion + '"';
  if Encoding <> '' then
    St := St + ' encoding="' + Encoding + '"';
  if StandAlone then
    St := St + ' standalone="yes"';
  St := St + '?>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== { TJvSimpleXMLElemDocType } ============================================

procedure TJvSimpleXMLElemDocType.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
{
<!DOCTYPE test [
<!ELEMENT test (#PCDATA) >
<!ENTITY % xx '&#37;zz;'>
<!ENTITY % zz '&#60;!ENTITY tricky "error-prone" >' >
%xx;
]>

<!DOCTYPE greeting SYSTEM "hello.dtd">
}
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
  lChar: Char;
  St: string;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;
  lChar := '>';
  St := '';

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<!DOCTYPE
          if lBuf[I] = CS_START_DOCTYPE[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_DOCTYPE[lPos], lBuf[I]]);
        10: //]> or >
          if lChar = lBuf[I] then
          begin
            if lChar = '>' then
            begin
              lOk := True;
              Count := 0;
              Break; //This is the end
            end
            else
            begin
              St := St + lBuf[I];
              lChar := '>';
            end;
          end
          else
          begin
            St := St + lBuf[I];
            if lBuf[I] = '[' then
              lChar := ']';
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Name := '';
  Value := Trim(St);

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemDocType.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXML);
var
  St: string;
begin
  St := '<!DOCTYPE ' + Value + '>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== { TJvSimpleXMLElemSheet } ==============================================

procedure TJvSimpleXMLElemSheet.LoadFromStream(const Stream: TStream;
  Parent: TJvSimpleXML);
//<?xml-stylesheet alternate="yes" type="text/xsl" href="sheet.xsl"?>
const
  CS_START_PI = '<?xml-stylesheet';
  CS_STOP_PI = '                ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..15: //<?xml-stylesheet
          if lBuf[I] = CS_START_PI[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_START_PI[lPos], lBuf[I]]);
        16: //L
          if lBuf[I] = CS_START_PI[lPos] then
          begin
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);
            Break; //Re read buffer
          end
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_START_PI[lPos], lBuf[I]]);
        17: //?
          if lBuf[I] = CS_STOP_PI[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_STOP_PI[lPos], lBuf[I]]);
        18: //>
          if lBuf[I] = CS_STOP_PI[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_STOP_PI[lPos], lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidStylesheetUnexpectedEndOfDat);

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemSheet.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXML);
var
  I: Integer;
  St: string;
begin
  St := Level + '<?xml-stylesheet';
  for I := 0 to Properties.GetCount - 1 do
    St := St + Properties.Item[I].SaveToString;
  St := St + '?>' + sLineBreak;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== { TJvSimpleXMLElemsProlog } ============================================

constructor TJvSimpleXMLElemsProlog.Create;
begin
  inherited Create;
  FElems := THashedStringList.Create;
end;

destructor TJvSimpleXMLElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TJvSimpleXMLElemsProlog.Clear;
var
  I: Integer;
begin
  for I := 0 to FElems.Count - 1 do
  begin
    FElems.Objects[I].Free;
    FElems.Objects[I] := nil;
  end;
  FElems.Clear;
end;

function TJvSimpleXMLElemsProlog.GetCount: Integer;
begin
  Result := FElems.Count;
end;

function TJvSimpleXMLElemsProlog.GetItem(const Index: Integer): TJvSimpleXMLElem;
begin
  Result := TJvSimpleXMLElem(FElems.Objects[Index]);
end;

function TJvSimpleXMLElemsProlog.LoadFromStream(
  const Stream: TStream; Parent: TJvSimpleXML): string;
{<?xml version="1.0" encoding="UTF-8" ?>
<!-- Test -->
<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>
<greeting>Hello, world!</greeting>

<?xml version="1.0"?> <!DOCTYPE greeting SYSTEM "hello.dtd"> <greeting>Hello, world!</greeting>
}
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lEnd: Boolean;
  lElem: TJvSimpleXMLElem;
begin
  lStreamPos := Stream.Position;
  Result := '';
  St := '';
  lPos := 0;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    if Parent <> nil then
      Parent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces
          begin
            case lBuf[I] of
              ' ', Tab, Cr, Lf:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  St := lBuf[I];
                end;
            else
              Error(RsEInvalidDocumentUnexpectedTextInFile);
            end;
          end;
        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            lEnd := False;

            St := St + lBuf[I];
            if St = '<![CDATA[' then
              lEnd := True
            else
            if St = '<!--' then
              lElem := TJvSimpleXMLElemComment.Create(nil)
            else
            if St = '<?xml-stylesheet' then
              lElem := TJvSimpleXMLElemSheet.Create(nil)
            else
            if St = '<?xml ' then
              lElem := TJvSimpleXMLElemHeader.Create(nil)
            else
            if St = '<!DOCTYPE' then
              lElem := TJvSimpleXMLElemDocType.Create(nil)
            else
            if (Length(St) > 1) and not (St[2] in ['!', '?']) then
              lEnd := True;

            if lEnd then
            begin
              lStreamPos := lStreamPos - Length(St);
              Count := 0;
              Break;
            end
            else
            if lElem <> nil then
            begin
              Stream.Seek(lStreamPos - (Length(St)), soFromBeginning);
              lElem.LoadFromStream(Stream);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              St := '';
              lPos := 0;
              Break;
            end;
          end;
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemsProlog.SaveToStream(const Stream: TStream; Parent: TJvSimpleXML);
var
  I: Integer;
begin
  FindHeader;
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, '', Parent);
end;

//=== { TJvSimpleHashTable } =================================================

constructor TJvSimpleHashTable.Create;
begin
  inherited Create;
  //XXX
  New(FList);
  FList^.Count := 0;
  FList^.Kind := hkDirect;
  FList^.FirstElem := nil;
end;

destructor TJvSimpleHashTable.Destroy;
begin
  Clear;
  Dispose(FList);
  inherited Destroy;
end;

procedure TJvSimpleHashTable.AddObject(const AName: string;
  AObject: TObject);
begin
  //XXX
  New(FList^.FirstElem);
  //FList^.FirstElem^.Value := AName;
  //FList^.FirstElem^.Obj := nil;
end;

procedure TJvSimpleHashTable.Clear;
begin
  //XXX
end;

{$IFDEF COMPILER6_UP}

function VarXML: TVarType;
begin
  Result := XMLVariant.VarType;
end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJvSimpleXMLElem);
begin
  TXMLVarData(ADest).vType := VarXML;
  TXMLVarData(ADest).XML := AXML;
end;

function XMLCreate(const AXML: TJvSimpleXMLElem): Variant;
begin
  XMLCreateInto(Result, AXML);
end;

function XMLCreate: Variant;
begin
  XMLCreateInto(Result, TJvSimpleXMLElemClassic.Create(nil));
end;

//=== { TXMLVariant } ========================================================

procedure TXMLVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if Source.vType = VarType then
  begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TXMLVarData(Source).XML.SaveToString);
      varString:
        VarDataFromStr(Dest, TXMLVarData(Source).XML.SaveToString);
    else
      RaiseCastError;
    end;
  end
  else
    inherited;
end;

procedure TXMLVariant.Clear(var V: TVarData);
begin
  V.vType := varEmpty;
  TXMLVarData(V).XML := nil;
end;

procedure TXMLVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TXMLVarData(Dest) do
    begin
      vType := VarType;
      XML := TXMLVarData(Source).XML;
    end;
end;

function TXMLVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LXML: TJvSimpleXMLElem;
  I, J, K: Integer;
begin
  Result := False;
  if (Length(Arguments) = 1) and (Arguments[0].vType in [vtInteger, vtExtended]) then
    with TXMLVarData(V) do
    begin
      K := Arguments[0].vInteger;
      J := 0;

      if K > 0 then
        for I := 0 to XML.Items.Count - 1 do
          if UpperCase(XML.Items[I].Name) = Name then
          begin
            Inc(J);
            if J = K then
              Break;
          end;

      if (J = K) and (J < XML.Items.Count) then
      begin
        LXML := XML.Items[J];
        if LXML <> nil then
        begin
          Dest.vType := VarXML;
          TXMLVarData(Dest).XML := LXML;
          Result := True;
        end
      end;
    end;
end;

function TXMLVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  LXML: TJvSimpleXMLElem;
  lProp: TJvSimpleXMLProp;
begin
  Result := False;
  with TXMLVarData(V) do
  begin
    LXML := XML.Items.ItemNamed[Name];
    if LXML <> nil then
    begin
      Dest.vType := VarXML;
      TXMLVarData(Dest).XML := LXML;
      Result := True;
    end
    else
    begin
      lProp := XML.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        VarDataFromOleStr(Dest, lProp.Value);
        Result := True;
      end;
    end;
  end;
end;

function TXMLVariant.IsClear(const V: TVarData): Boolean;
begin
  Result := (TXMLVarData(V).XML = nil) or
    (TXMLVarData(V).XML.Items.Count = 0);
end;

function TXMLVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  LXML: TJvSimpleXMLElem;
  lProp: TJvSimpleXMLProp;

  function GetStrValue: string;
  begin
    try
      Result := Value.VOleStr;
    except
      Result := '';
    end;
  end;

begin
  Result := False;
  with TXMLVarData(V) do
  begin
    LXML := XML.Items.ItemNamed[Name];
    if LXML = nil then
    begin
      lProp := XML.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        lProp.Value := GetStrValue;
        Result := True;
      end;
    end
    else
    begin
      LXML.Value := GetStrValue;
      Result := True;
    end;
  end;
end;

{$ENDIF COMPILER6_UP}

procedure TJvSimpleXMLElemsProlog.Error(const S: string);
begin
  raise EJvSimpleXMLError.Create(S);
end;

procedure TJvSimpleXMLElemsProlog.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

procedure TJvSimpleXML.SetIndentString(const Value: string);
var
  I: Integer;
begin
  // test if the new value is only made of spaces or tabs
  for I := 0 to Length(Value) do
    if not (Value[I] in [Tab, ' ']) then
      Exit;
  FIndentString := Value;
end;

procedure TJvSimpleXML.SetRoot(const Value: TJvSimpleXMLElemClassic);
begin
  if Value <> FRoot then
  begin
//    FRoot.FSimpleXML := nil;
    FRoot := Value;
//    FRoot.FSimpleXML := Self;
  end;
end;

function TJvSimpleXMLElemsProlog.GetEncoding: string;
var
  Elem: TJvSimpleXMLElemHeader;
begin
  Elem := TJvSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Encoding
  else
    Result := 'UTF-8';
end;

function TJvSimpleXMLElemsProlog.GetStandAlone: Boolean;
var
  Elem: TJvSimpleXMLElemHeader;
begin
  Elem := TJvSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.StandAlone
  else
    Result := False;
end;

function TJvSimpleXMLElemsProlog.GetVersion: string;
var
  Elem: TJvSimpleXMLElemHeader;
begin
  Elem := TJvSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Version
  else
    Result := '1.0';
end;

procedure TJvSimpleXMLElemsProlog.SetEncoding(const Value: string);
var
  Elem: TJvSimpleXMLElemHeader;
begin
  Elem := TJvSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Encoding := Value;
end;

procedure TJvSimpleXMLElemsProlog.SetStandAlone(const Value: Boolean);
var
  Elem: TJvSimpleXMLElemHeader;
begin
  Elem := TJvSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.StandAlone := Value;
end;

procedure TJvSimpleXMLElemsProlog.SetVersion(const Value: string);
var
  Elem: TJvSimpleXMLElemHeader;
begin
  Elem := TJvSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Version := Value;
end;

function TJvSimpleXMLElemsProlog.FindHeader: TJvSimpleXMLElem;
var
  I: Integer;
begin
  if Count = 0 then
  begin
    Result := TJvSimpleXMLElemHeader.Create(nil);
    FElems.AddObject('', Result);
  end
  else
  begin
    for I := 0 to Count - 1 do
      if Item[I] is TJvSimpleXMLElemHeader then
      begin
        Result := Item[I];
        Exit;
      end;
    Result := nil;
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
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF COMPILER6_UP}
  FreeAndNil(GlobalXMLVariant);
  {$ENDIF COMPILER6_UP}
  FreeAndNil(GlobalSorts);
  
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

