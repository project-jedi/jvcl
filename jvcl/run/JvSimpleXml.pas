{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimpleXML.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Christophe Paris.

Last Modified: 2003-08-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues: This component does not parse the !DOCTYPE tags but preserves them
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSimpleXML;

interface

uses
  SysUtils, Classes, IniFiles, JvComponent{$IFDEF COMPILER6_UP}, Variants{$ENDIF};

type
{$IFNDEF COMPILER6_UP}
  THashedStringList = class(TStringList);
  THandle = Longword;
{$ENDIF}
  TJvSimpleXML = class;
  TJvSimpleXMLInvalid = class(Exception);
  TJvSimpleXMLElem = class;
  TJvSimpleXMLElems = class;
  TJvSimpleXMLProps = class;
  TJvSimpleXMLElemComment = class;
  TJvSimpleXMLElemClassic = class;
  TJvSimpleXMLElemCData = class;
  TJvSimpleXMLElemText = class;
  TJvOnSimpleXmlParsed = procedure(Sender: TObject; Name: string) of object;
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
  TJvHashList = array[0..25] of PJvHashRecord;
  PJvHashList = ^TJvHashList;
  TJvHashRecord = packed record
    Count: Byte;
    case Kind: TJvHashKind of
      hkList: (List: PJvHashList);
      hkDirect: (FirstElem: PJvHashElem);
  end;

  TJvSimpleHashTable = class
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
  protected
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    function SaveToString: string;
    property Parent: TJvSimpleXMLProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property Pointer: string read FPointer write FPointer;

    property Data: Pointer read FData write FData;
  end;

  TJvSimpleXMLProps = class(TObject)
  private
    FProperties: THashedStringList;
    FParent : TJvSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXMLProp;
  protected
    function GetItem(const Index: Integer): TJvSimpleXMLProp;
    procedure DoItemRename(var Value: TJvSimpleXMLProp; const Name: string);
    procedure Error(const S:string);
    procedure FmtError(const S:string;const Args:array of const);
  public
    constructor Create(Parent : TJvSimpleXMLElem); reintroduce; overload;
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
  protected
    procedure Error(const S:string);
    procedure FmtError(const S:string;const Args:array of const);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil): string;
    procedure SaveToStream(const Stream: TStream; Parent: TJvSimpleXML = nil);
    property Item[const Index: Integer]: TJvSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
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
    function Add(const Name: string): TJvSimpleXMLElemClassic; overload;
    function Add(const Name, Value: string): TJvSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: TStream): TJvSimpleXMLElemClassic; overload;
    function Add(Value: TJvSimpleXMLElem): TJvSimpleXMLElem; overload;
    function AddFirst(Value: TJvSimpleXMLElem): TJvSimpleXMLElem; overload;
    function AddFirst(const Name: string): TJvSimpleXMLElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TJvSimpleXMLElemComment;
    function AddCData(const Name: string; const Value: string): TJvSimpleXMLElemCDATA;
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
    FSimpleXml : TJvSimpleXML;
    function GetSimpleXml: TJvSimpleXML;
  protected
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TJvSimpleXMLProps;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
    function GetItems: TJvSimpleXMLElems;
    procedure Error(const S:string);
    procedure FmtError(const S:string;const Args:array of const);
  public
    constructor Create(const AOwner: TJvSimpleXMLElem);
    destructor Destroy; override;
    procedure Assign(Value: TJvSimpleXMLElem);
    procedure Clear; virtual;
    function SaveToString: string;
    procedure LoadFromString(Value : string);
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); virtual; abstract;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); virtual;
      abstract;
    procedure GetBinaryValue(const Stream: TStream);
    property Data: Pointer read FData write FData;
    function GetChildIndex(const AChild: TJvSimpleXMLElem): Integer;

    property SimpleXml : TJvSimpleXML read GetSimpleXml;
  published
    property Name: string read FName write SetName;
    property Parent: TJvSimpleXMLElem read FParent write FParent;
    property Pointer: string read FPointer write FPointer;
    property ChildsCount: Integer read GetChildsCount;
    property Items: TJvSimpleXMLElems read GetItems;
    property Properties: TJvSimpleXMLProps read GetProps;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
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
    constructor Create;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXML = nil); override;
    property Version: string read FVersion write FVersion;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;
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

  TJvSimpleXMLOptions = set of (sxoAutoCreate, sxoAutoIndent);

  TJvSimpleXML = class(TJvComponent)
  private
    FIndentString: string;
    procedure SetIndentString(const Value: string);
  protected
    FFileName: TFileName;
    FOptions : TJvSimpleXMLOptions;
    FRoot: TJvSimpleXMLElemClassic;
    FOnTagParsed: TJvOnSimpleXmlParsed;
    FOnValue: TJvOnValueParsed;
    FOnLoadProg: TJvOnSimpleProgress;
    FOnSaveProg: TJvOnSimpleProgress;
    FProlog: TJvSimpleXMLElemsProlog;
    FSaveCount, FSaveCurrent: Integer;
    procedure SetRoot(const Value: TJvSimpleXMLElemClassic);
    procedure SetFileName(Value: TFileName);
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName);
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToStream(const Stream: TStream);
    function SaveToString: string;
    property Prolog: TJvSimpleXMLElemsProlog read FProlog write FProlog;
    property Root: TJvSimpleXMLElemClassic read FRoot write SetRoot;
  published
    property FileName: TFileName read FFileName write SetFileName;
    property IndentString : string read FIndentString write SetIndentString;
    property Options : TJvSimpleXMLOptions read FOptions write FOptions;
    property OnSaveProgress: TJvOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress: TJvOnSimpleProgress read FOnLoadProg write FOnLoadProg;
    property OnTagParsed: TJvOnSimpleXmlParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TJvOnValueParsed read FOnValue write FOnValue;
  end;

{$IFDEF COMPILER6_UP}

  TXmlVariant = class(TInvokeableVariantType)
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

  TXmlVarData = packed record
    VType: TVarType;
    Reserved1: Word;
    Reserved2: Word;
    Reserved3: Word;
    Xml: TJvSimpleXMLElem;
    Reserved4: Longint;
  end;

procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXMLElem);
function XmlCreate(const AXml: TJvSimpleXMLElem): Variant; overload;
function XmlCreate: Variant; overload;
function VarXml: TVarType;

{$ENDIF}
function SimpleXmlEncode(const Value: string): string;
procedure SimpleXmlDecode(var Value: string; TrimMultiple: Boolean = True);

resourcestring
  RS_INVALID_SimpleXml = 'Invalid XML file';
{$IFNDEF COMPILER6_UP}
  SInvalidBoolean = '''%s'' is not a valid Boolean value';
{$ENDIF COMPILER6_UP}


resourcestring
  sInvalidXMLElementUnexpectedCharacte = 'Invalid XML Element: Unexpected character in properties declaration ("%s" found).';
  sInvalidXMLElementUnexpectedCharacte_ = 'Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but "%s"  found.';
  sUnexpectedValueForLPos = 'Unexpected value for lPos';
  sInvalidXMLElementExpectedBeginningO = 'Invalid XML Element: Expected beginning of tag but "%s" found.';
  sInvalidXMLElementExpectedEndOfTagBu = 'Invalid XML Element: Expected end of tag but "%s" found.';
  sInvalidXMLElementMalformedTagFoundn = 'Invalid XML Element: malformed tag found (no valid name)';
  sInvalidXMLElementErroneousEndOfTagE = 'Invalid XML Element: Erroneous end of tag, expecting </%s> but </%s> found.';
  sInvalidCommentExpectedsButFounds = 'Invalid Comment: expected "%s" but found "%s"';
  sInvalidCommentNotAllowedInsideComme = 'Invalid Comment: "--" not allowed inside comments';
  sInvalidCommentUnexpectedEndOfData = 'Invalid Comment: Unexpected end of data';
  sInvalidCDATAExpectedsButFounds = 'Invalid CDATA: expected "%s" but found "%s"';
  sInvalidCDATAUnexpectedEndOfData = 'Invalid CDATA: Unexpected end of data';
  sInvalidHeaderExpectedsButFounds = 'Invalid Header: expected "%s" but found "%s"';
  sInvalidStylesheetExpectedsButFounds = 'Invalid Stylesheet: expected "%s" but found "%s"';
  sInvalidStylesheetUnexpectedEndOfDat = 'Invalid Stylesheet: Unexpected end of data';
  sInvalidDocumentUnexpectedTextInFile = 'Invalid Document: Unexpected text in file prolog.';

implementation

uses
  JvConsts, JvTypes;

const
  cBufferSize = 8192;

{$IFDEF COMPILER6_UP}
var
  XmlVariant: TXmlVariant = nil;
{$ENDIF}
var
  GSorts: TList = nil;
{$IFNDEF COMPILER6_UP}

var
  TrueBoolStrs: array of string;
  FalseBoolStrs: array of string;

const
  DefaultTrueBoolStr = 'True'; // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

{$ENDIF COMPILER6_UP}

{$IFNDEF COMPILER6_UP}

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

procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const);
begin
  raise EConvertError.CreateResFmt(ResString, Args);
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;

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

var
  LResult: Extended;
begin
  Result := TryStrToFloat(S, LResult);
  if Result then
    Value := LResult <> 0
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

function StrToBool(S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    ConvertErrorFmt(@SInvalidBoolean, [S]);
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array[Boolean] of string = ('0', '-1');
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

{$ENDIF COMPILER6_UP}

function SimpleXmlEncode(const Value: string): string;
var
  i: Integer;
  lDiff: Boolean;
begin
  //http://www.cs.tut.fi/~jkorpela/latin1/3.html#60
  result := Value;
  lDiff := false;
  for i := 1 to Length(Value) do
    if Value[i] in ['<', '>', '&', '"', ''''] then
    begin
      if not lDiff then
      begin
        lDiff := true;
        result := Copy(Value, 1, i - 1);
      end;
      result := result + '&#' + IntToStr(Ord(Value[i])) + ';';
    end
    else
      if lDiff then
        result := result + Value[i];
end;

procedure SimpleXmlDecode(var Value: string; TrimMultiple: Boolean = True);
var
  i, j, k, l: Integer;
  st: string;
begin
  st := '';
  j := -1;
  k := 1;
  for i := 1 to Length(Value) do
    case Value[i] of
      ' ', #10, #13:
        if (not TrimMultiple) or ((k = 1) or not (Value[k - 1] in [' ', #10, #13])) then
        begin
          if j > 0 then
            st := st + Value[i]
          else
          begin
            Value[k] := Value[i];
            inc(k);
          end;
        end;
      '&':
        begin
          if j <> -1 then
          begin
            Value[k] := '&';
            inc(k);
            for l := 1 to Length(st) do
            begin
              Value[k] := st[l];
              inc(k);
            end;
          end;
          j := 0;
          st := '';
        end;
      '#':
        if j = 0 then
          j := 1
        else if (j <> -1) then
        begin
          Value[k] := '&';
          inc(k);
          for l := 1 to Length(st) do
          begin
            Value[k] := st[l];
            inc(k);
          end;
          Value[k] := Value[i];
          inc(k);
          st := '';
        end
        else
        begin
          for l := 1 to Length(st) do
          begin
            Value[k] := st[l];
            inc(k);
          end;
          Value[k] := Value[i];
          inc(k);
          st := '';
        end;
      '0'..'9':
        if j >= 1 then
        begin
          st := st + Value[i];
          j := 2;
        end
        else
        begin
          Value[k] := Value[i];
          inc(k);
        end;
      'a'..'z', 'A'..'Z':
        if j >= 0 then
        begin
          st := st + Value[i];
          inc(j);
        end
        else
        begin
          Value[k] := Value[i];
          inc(k);
        end;
      ';':
        if j <> 0 then
        begin
          j := StrToIntDef(st, -1);
          case j of
            -1:
              begin
                st := LowerCase(st);
                if st = 'lt' then
                begin
                  Value[k] := '<';
                  inc(k);
                end
                else if st = 'gt' then
                begin
                  Value[k] := '>';
                  inc(k);
                end
                else if st = 'amp' then
                begin
                  Value[k] := '&';
                  inc(k);
                end
                else
                begin
                  if st <> '' then
                  begin
                    Value[k] := '&';
                    inc(k);
                    for l := 1 to Length(st) do
                    begin
                      Value[k] := st[l];
                      inc(k);
                    end;
                    st := '';
                  end;
                  Value[k] := ';';
                  inc(k);
                end
              end;
            0..100:
              begin
                Value[k] := Char(j);
                inc(k);
              end;
            233:
              begin
                Value[k] := 'é';
                inc(k);
              end;
            232:
              begin
                Value[k] := 'è';
                inc(k);
              end;
          end;
          st := '';
          j := -1;
        end
        else
        begin
          Value[k] := Value[i];
          inc(k);
        end;
    else
      begin
        if j > 0 then
        begin
          Value[k] := '&';
          inc(k);
          for l := 1 to Length(st) do
          begin
            Value[k] := st[l];
            inc(k);
          end;
        end
        else if j = 0 then
        begin
          Value[k] := '&';
          inc(k);
        end;
        Value[k] := Value[i];
        inc(k);
        j := -1;
      end;
    end;
  if j <> -1 then
  begin
    Value[k] := '&';
    inc(k);
    for l := 1 to Length(st) do
    begin
      Value[k] := st[l];
      inc(k);
    end;
  end;
  SetLength(Value, k - 1);
end;

//=== TJvSimpleXML ===========================================================

constructor TJvSimpleXML.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoot := TJvSimpleXMLElemClassic.Create(nil);
  FRoot.FSimpleXml := Self;
  FProlog := TJvSimpleXMLElemsProlog.Create;
  FOptions := [sxoAutoIndent];
  FIndentString := '  ';
end;

destructor TJvSimpleXML.Destroy;
begin
  FRoot.Free;
  FProlog.Free;
  inherited Destroy;
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

procedure TJvSimpleXML.LoadFromStream(const Stream: TStream);
begin
  FRoot.Clear;
  FProlog.Clear;
  if Assigned(FOnLoadProg) then
  begin
    FOnLoadProg(Self, Stream.Position, Stream.Size);
    //Read doctype and so on
    FProlog.LoadFromStream(Stream, Self);
    //Read elements
    FRoot.LoadFromStream(Stream, Self);
    FOnLoadProg(Self, Stream.Position, Stream.Size);
  end
  else
  begin
    if Assigned(FOnTagParsed) or Assigned(FOnValue) then
    begin
      FProlog.LoadFromStream(Stream, Self);
      FRoot.LoadFromStream(Stream, Self);
    end
    else
    begin
      FProlog.LoadFromStream(Stream);
      FRoot.LoadFromStream(Stream);
    end;
  end;
end;

procedure TJvSimpleXML.LoadFromString(const Value: string);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(Value);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
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

procedure TJvSimpleXML.SaveToStream(const Stream: TStream);
var
  lCount: Integer;
begin
  if Assigned(FOnSaveProg) then
  begin
    lCount := Root.ChildsCount + Prolog.Count;
    FSaveCount := lCount;
    FSaveCurrent := 0;
    FOnSaveProg(Self, 0, lCount);
    Prolog.SaveToStream(Stream, Self);
    Root.SaveToStream(Stream, '', Self);
    FOnSaveProg(Self, lCount, lCount);
  end
  else
  begin
    Prolog.SaveToStream(Stream);
    Root.SaveToStream(Stream);
  end;
end;

function TJvSimpleXML.SaveToString: string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('');
  try
    SaveToStream(LStream);
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

procedure TJvSimpleXML.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== TJvSimpleXMLElem =======================================================

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
end;

destructor TJvSimpleXMLElem.Destroy;
begin
  Clear;
  FItems.Free;
  FProps.Free;
  inherited Destroy;
end;

procedure TJvSimpleXMLElem.Error(const S: string);
begin
  raise TJvSimpleXMLInvalid.Create(S);
end;

procedure TJvSimpleXMLElem.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S,Args));
end;

procedure TJvSimpleXMLElem.GetBinaryValue(const Stream: TStream);
var
  I, J: Integer;
  St: string;
  Buf: array[0..cBufferSize - 1] of Byte;
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
    FProps := TJvSimpleXMLProps.Create(self);
  Result := FProps;
end;

function TJvSimpleXMLElem.GetSimpleXml: TJvSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.SimpleXml
  else
    Result := FSimpleXml;
end;

procedure TJvSimpleXMLElem.LoadFromString(Value: string);
var
  lStream : TStringStream;
begin
  lStream := TStringStream.Create(Value);
  try
    LoadFromStream(lStream);
  finally
    lStream.Free;
  end;
end;

function TJvSimpleXMLElem.SaveToString: string;
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  try
    SaveToStream(lStream);
    Result := lStream.DataString;
  finally
    lStream.Free;
  end;
end;

procedure TJvSimpleXMLElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
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

//=== TJvSimpleXMLElems ======================================================

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
  Buf: array[0..cBufferSize - 1] of Byte;
  St: string;
  I, Count: Integer;
begin
  Stream := TStringStream.Create('');
  repeat
    St := '';
    Count := Value.Read(Buf, SizeOf(Buf));
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
  FElems.AddObject(Value.Name, Value);
end;

procedure TJvSimpleXMLElems.AddChildFirst(const Value: TJvSimpleXMLElem);
begin
  CreateElems;
  FElems.InsertObject(0, Value.Name, Value)
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

function TJvSimpleXMLElems.AddCData(const Name, Value: string): TJvSimpleXMLElemCDATA;
begin
  Result := TJvSimpleXMLElemCDATA.Create(Parent);
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
      // TJvSimpleXMLElem(FElems.Objects[I]).Clear; // (p3) not needed -called in Destroy
      TJvSimpleXMLElem(FElems.Objects[I]).Free;
    FElems.Clear;
  end;
end;

constructor TJvSimpleXMLElems.Create(const AOwner: TJvSimpleXMLElem);
begin
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
  Clear;
  if FElems <> nil then
    FElems.Free;
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
    else if Assigned(Parent) and
      Assigned(Parent.SimpleXml) and
      (sxoAutoCreate in Parent.SimpleXml.Options) then
      Result := Add(Name);
  end
  else if Assigned(Parent) and
      Assigned(Parent.SimpleXml) and
      (sxoAutoCreate in Parent.SimpleXml.Options) then
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
  lBuf: array[0..cBufferSize - 1] of Char;
  St: string;
  lElem: TJvSimpleXMLElem;
begin
  lStreamPos := Stream.Position;
  Result := '';
  St := '';
  lPos := 0;

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
              ' ', #9, #13, #10:
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
  i: Integer;
begin
  Result := 0;
  for i := 0 to GSorts.Count - 1 do
    if TJvSimpleXMLElems(GSorts[i]).FElems = List then
    begin
      Result := TJvSimpleXMLElems(GSorts[i]).FCompare(TJvSimpleXMLElems(GSorts[i]), Index1, Index2);
      Exit;
    end;
end;

procedure TJvSimpleXMLElems.CustomSort(
  AFunction: TJvSimpleXMLElemCompare);
begin
  if FElems <> nil then
  begin
    GSorts.Add(self);
    FCompare := AFunction;
    FElems.CustomSort(SortItems);
    GSorts.Remove(self);
  end;
end;

procedure TJvSimpleXMLElems.Sort;
begin
  if FElems <> nil then
    FElems.Sort;
end;


//=== TJvSimpleXMLProps ======================================================

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
      TJvSimpleXMLProp(FProperties.Objects[I]).Free;
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
  Clear;
  FProperties.Free;
  inherited Destroy;
end;

procedure TJvSimpleXMLProps.DoItemRename(var Value: TJvSimpleXMLProp;
  const Name: string);
var
  I: Integer;
begin
  if FProperties = nil then Exit;
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

procedure TJvSimpleXMLProps.Error(const S: string);
begin
  raise TJvSimpleXMLInvalid.Create(S);
end;

procedure TJvSimpleXMLProps.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S,Args));
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
    else if Assigned(FParent) and
      Assigned(FParent.SimpleXml) and
      (sxoAutoCreate in FParent.SimpleXml.Options) then
      Result := Add(Name, '');
  end
  else if Assigned(FParent) and
    Assigned(FParent.SimpleXml) and
    (sxoAutoCreate in FParent.SimpleXml.Options) then
  begin
    Result := Add(Name, '');
  end;
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
  lBuf: array [0..cBufferSize-1] of Char;
  lName, lValue, lPointer: string;
  lPropStart: Char;
begin
  lStreamPos := Stream.Position;
  lValue := '';
  lPointer := '';
  lName := '';
  lPropStart := ' ';
  lPos := ptWaiting;

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
              ' ', #9, #10, #13:
                begin
                end;
              'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
                begin
                  lName := lBuf[I];
                  lPos := ptReadingName;
                end;
              '/', '>', '?':
                begin
                  Dec(lStreamPos);
                  Count := 0;
                  Break;
                end;
            else
              FmtError(sInvalidXMLElementUnexpectedCharacte,[lBuf[I]]);
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
            ' ', #9, #10, #13:
              lPos := ptSpaceBeforeEqual;
          else
            FmtError(sInvalidXMLElementUnexpectedCharacte,[lBuf[I]]);
          end;

        ptStartingContent: //We are going to start a property content
          case lBuf[I] of
            ' ', #9, #10, #13:
              ; // ignore white space
            '''', '"':
              begin
                lPropStart := lBuf[I];
                lValue := '';
                lPos := ptReadingValue;
              end;
          else
            FmtError(sInvalidXMLElementUnexpectedCharacte_,[lBuf[I]]);
          end;
        ptReadingValue: //We are reading a property
          if lBuf[I] = lPropStart then
          begin
            SimpleXmlDecode(lValue, False);
            with Add(lName, lValue) do
              Pointer := lPointer;
            lPos := ptWaiting;
          end
          else
            lValue := lValue + lBuf[I];
        ptSpaceBeforeEqual: // We are reading the white space between a property name and the = sign
          case lBuf[I] of
            ' ', #9, #10, #13:
              ; // more white space, stay in this state and ignore
            '=':
              lPos := ptStartingContent;
          else
            FmtError(sInvalidXMLElementUnexpectedCharacte,[lBuf[I]]);
          end;
      else
        Assert(False, sUnexpectedValueForLPos);
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

//=== TJvSimpleXMLProp =======================================================

function TJvSimpleXMLProp.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJvSimpleXMLProp.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJvSimpleXMLProp.SaveToString: string;
begin
  if Pointer <> '' then
    Result := Format(' %s:%s="%s"', [Pointer, Name, SimpleXmlEncode(Value)])
  else
    Result := Format(' %s="%s"', [Name, SimpleXmlEncode(Value)]);
end;

procedure TJvSimpleXMLProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
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

//=== TJvSimpleXMLElemClassic ================================================

procedure TJvSimpleXMLElemClassic.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array[0..cBufferSize - 1] of Char;
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
            FmtError(sInvalidXMLElementExpectedBeginningO, [lBuf[I]]);
        -1:
          if lBuf[I] = '>' then
          begin
            Count := 0;
            Break;
          end
          else
            FmtError(sInvalidXMLElementExpectedEndOfTagBu,[lBuf[I]]);
      else
        begin
          if lBuf[I] in [#9, #10, #13, ' ', '.'] then
          begin
            if lPos = 2 then
              Error(sInvalidXMLElementMalformedTagFoundn);
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
                    FmtError(sInvalidXMLElementErroneousEndOfTagE,[lName,St]);
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
  SimpleXmlDecode(lValue);
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
  St: string;
  LevelAdd : string;
begin
  if Name <> '' then
  begin
    St := Level + '<' + Name;
    Stream.Write(St[1], Length(St));
    Properties.SaveToStream(Stream);
  end;

  if (Items.Count = 0) then
  begin
    if (Name <> '') then
    begin
      if Value = '' then
        St := '/>' + CrLf
      else
       St := '>' + SimpleXmlEncode(Value) + '</' + Name + '>' + CrLf;
      Stream.Write(St[1], Length(St));
    end;
  end
  else
  begin
    if (Name <> '') then
    begin
      St := '>' + CrLf;
      Stream.Write(St[1], Length(St));
    end;
    if Assigned(SimpleXml) and
      (sxoAutoIndent in SimpleXml.Options) then
    begin
      LevelAdd := SimpleXml.IndentString;
    end;
    Items.SaveToStream(Stream, Level + LevelAdd, Parent);
    if Name <> '' then
    begin
      St := Level + '</' + Name + '>' + CrLf;
      Stream.Write(St[1], Length(St));
    end;
  end;
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXMLElemComment ================================================

procedure TJvSimpleXMLElemComment.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
//<!-- declarations for <head> & <body> -->
const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT = '    -->';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array[0..cBufferSize - 1] of Char;
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
            FmtError(sInvalidCommentExpectedsButFounds,[CS_START_COMMENT[lPos],lBuf[I]]);
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
            if lBuf[i+1] <> '>' then
              Error(sInvalidCommentNotAllowedInsideComme);
            St := St + '--' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(sInvalidCommentUnexpectedEndOfData);

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
  St := '-->' + CrLf;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXMLElemCData ==================================================

procedure TJvSimpleXMLElemCData.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA = '         ]]>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array[0..cBufferSize - 1] of Char;
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
            FmtError(sInvalidCDATAExpectedsButFounds,[CS_START_CDATA[lPos],lBuf[I]]);
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
    Error(sInvalidCDATAUnexpectedEndOfData);

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
  St := ']]>' + CrLf;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXMLElemText ===================================================

procedure TJvSimpleXMLElemText.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXML);
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array[0..cBufferSize - 1] of Char;
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

  SimpleXmlDecode(St);
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
    St := Level + SimpleXmlEncode(Value) + CrLf;
    Stream.Write(St[1], Length(St));
  end;
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXMLElemHeader =================================================

constructor TJvSimpleXMLElemHeader.Create;
begin
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
  lBuf: array[0..cBufferSize - 1] of Char;
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
            FmtError(sInvalidHeaderExpectedsButFounds,[CS_START_HEADER[lPos],lBuf[I]]);
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
            FmtError(sInvalidHeaderExpectedsButFounds,[CS_START_HEADER[lPos],lBuf[I]]);
        6: //?
          if lBuf[I] = CS_STOP_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError(sInvalidHeaderExpectedsButFounds,[CS_STOP_HEADER[lPos],lBuf[I]]);
        7: //>
          if lBuf[I] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(sInvalidHeaderExpectedsButFounds,[CS_STOP_HEADER[lPos],lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(sInvalidCommentUnexpectedEndOfData);

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemHeader.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXML);
var
  St: string;
begin
  St := Level + '<?xml version="' + FVersion + '"';
  if Standalone then
    St := St + ' standalone="yes"';
  if Encoding <> '' then
    St := St + ' encoding="' + Encoding + '"';
  St := St + '?>' + CrLf;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXMLElemDocType ================================================

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
  lBuf: array[0..cBufferSize - 1] of Char;
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
            FmtError(sInvalidHeaderExpectedsButFounds,[CS_START_DOCTYPE[lPos],lBuf[I]]);
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
    Error(sInvalidCommentUnexpectedEndOfData);

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
  St := '<!DOCTYPE ' + Value + '>' + CrLf;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXMLElemSheet ==================================================

procedure TJvSimpleXMLElemSheet.LoadFromStream(const Stream: TStream;
  Parent: TJvSimpleXML);
//<?xml-stylesheet alternate="yes" type="text/xsl" href="sheet.xsl"?>
const
  CS_START_PI = '<?xml-stylesheet';
  CS_STOP_PI = '                ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array[0..cBufferSize - 1] of Char;
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
            FmtError(sInvalidStylesheetExpectedsButFounds,[CS_START_PI[lPos],lBuf[I]]);
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
            FmtError(sInvalidStylesheetExpectedsButFounds,[CS_START_PI[lPos],lBuf[I]]);
        17: //?
          if lBuf[I] = CS_STOP_PI[lPos] then
            Inc(lPos)
          else
            FmtError(sInvalidStylesheetExpectedsButFounds,[CS_STOP_PI[lPos],lBuf[I]]);
        18: //>
          if lBuf[I] = CS_STOP_PI[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(sInvalidStylesheetExpectedsButFounds,[CS_STOP_PI[lPos],lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(sInvalidStylesheetUnexpectedEndOfDat);

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXMLElemSheet.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXML);
var
  I: integer;
  St: string;
begin
  St := Level + '<?xml-stylesheet';
  for I := 0 to Properties.GetCount - 1 do
    St := St + Properties.Item[I].SaveToString;
  St := St + '?>' + CrLf;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXMLElemsProlog ================================================

constructor TJvSimpleXMLElemsProlog.Create;
begin
  inherited Create;
  FElems := THashedStringList.Create;
end;

destructor TJvSimpleXMLElemsProlog.Destroy;
begin
  Clear;
  FElems.Free;
  inherited Destroy;
end;

procedure TJvSimpleXMLElemsProlog.Clear;
var i: integer;
begin
  for i := 0 to FElems.Count - 1 do
    TJvSimpleXMLElem(FElems.Objects[i]).Free;
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
  lBuf: array[0..cBufferSize - 1] of Char;
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
              ' ', #9, #13, #10:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  St := lBuf[I];
                end;
            else
              Error(sInvalidDocumentUnexpectedTextInFile);
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
                    lElem := TJvSimpleXMLElemHeader.Create
                  else
                    if St = '<!DOCTYPE' then
                      lElem := TJvSimpleXMLElemDoctype.Create(nil)
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
  if Count = 0 then
    FElems.AddObject('', TJvSimpleXMLElemHeader.Create);
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, '', Parent);
end;

//=== TJvSimpleHashTable =====================================================

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

function VarXml: TVarType;
begin
  Result := XmlVariant.VarType;
end;

procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXMLElem);
begin
  TXmlVarData(ADest).VType := VarXml;
  TXmlVarData(ADest).Xml := AXml;
end;

function XmlCreate(const AXml: TJvSimpleXMLElem): Variant;
begin
  XmlCreateInto(Result, AXml);
end;

function XmlCreate: Variant;
begin
  XmlCreateInto(Result, TJvSimpleXMLElemClassic.Create(nil));
end;

//=== TXmlVariant ============================================================

procedure TXmlVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if Source.VType = VarType then
  begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TXmlVarData(Source).Xml.SaveToString);
      varString:
        VarDataFromStr(Dest, TXmlVarData(Source).Xml.SaveToString);
    else
      RaiseCastError;
    end;
  end
  else
    inherited;
end;

procedure TXmlVariant.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  TXmlVarData(V).Xml := nil;
end;

procedure TXmlVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TXmlVarData(Dest) do
    begin
      VType := VarType;
      Xml := TXmlVarData(Source).Xml;
    end;
end;

function TXmlVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  lXml: TJvSimpleXMLElem;
  I, J, K: Integer;
begin
  Result := False;
  if (Length(Arguments) = 1) and (Arguments[0].VType in [vtInteger, vtExtended]) then
    with TXmlVarData(V) do
    begin
      K := Arguments[0].vInteger;
      J := 0;

      if K > 0 then
        for I := 0 to Xml.Items.Count - 1 do
          if UpperCase(Xml.Items[I].Name) = Name then
          begin
            Inc(J);
            if J = K then
              Break;
          end;

      if (J = K) and (J < Xml.Items.Count) then
      begin
        lXml := Xml.Items[J];
        if lXml <> nil then
        begin
          Dest.VType := VarXml;
          TXmlVarData(Dest).Xml := lXml;
          Result := True;
        end
      end;
    end;
end;

function TXmlVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  lXml: TJvSimpleXMLElem;
  lProp: TJvSimpleXMLProp;
begin
  Result := False;
  with TXmlVarData(V) do
  begin
    lXml := Xml.Items.ItemNamed[Name];
    if lXml <> nil then
    begin
      Dest.VType := VarXml;
      TXmlVarData(Dest).Xml := lXml;
      Result := True;
    end
    else
    begin
      lProp := Xml.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        VarDataFromOleStr(Dest, lProp.Value);
        Result := True;
      end;
    end;
  end;
end;

function TXmlVariant.IsClear(const V: TVarData): Boolean;
begin
  Result := (TXmlVarData(V).Xml = nil) or
    (TXmlVarData(V).Xml.Items.Count = 0);
end;

function TXmlVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  lXml: TJvSimpleXMLElem;
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
  with TXmlVarData(V) do
  begin
    lXml := Xml.Items.ItemNamed[Name];
    if lXml = nil then
    begin
      lProp := Xml.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        lProp.Value := GetStrValue;
        Result := True;
      end;
    end
    else
    begin
      lXml.Value := GetStrValue;
      Result := True;
    end;
  end;
end;
{$ENDIF}

procedure TJvSimpleXMLElemsProlog.Error(const S: string);
begin
  raise TJvSimpleXMLInvalid.Create(S);
end;

procedure TJvSimpleXMLElemsProlog.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S,Args));
end;

procedure TJvSimpleXML.SetIndentString(const Value: string);
var
  ValueOk : Boolean;
  I : Integer;
begin
  // test if the new value is only made of spaces or tabs
  ValueOk := True;
  I := 0;
  while (I < Length(Value)) and ValueOk do
  begin
    Inc(I);
    ValueOk := (Value[I] = ' ') or (Value[I] = #8);
  end;
  if ValueOk then
    FIndentString := Value;
end;

procedure TJvSimpleXML.SetRoot(const Value: TJvSimpleXMLElemClassic);
begin
  if Value <> FRoot then
  begin
    FRoot.FSimpleXml := nil;
    FRoot := Value;
    FRoot.FSimpleXml := Self;
  end;
end;

initialization
{$IFDEF COMPILER6_UP}
  XmlVariant := TXmlVariant.Create;
{$ENDIF}
  GSorts := TList.Create;
finalization
{$IFDEF COMPILER6_UP}
  FreeAndNil(XmlVariant);
{$ENDIF}
  FreeAndNil(GSorts);
end.

