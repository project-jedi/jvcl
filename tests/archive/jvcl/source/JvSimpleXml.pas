{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimpleXml.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Christophe Paris.

Last Modified: 2002-10-22

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues: This component does not parse the !DOCTYPE tags but preserves them
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSimpleXml;

interface

uses
  SysUtils, Classes, IniFiles, JvComponent{$IFDEF COMPILER6_UP}, Variants{$ENDIF};

type
{$IFNDEF COMPILER6_UP}
  THashedStringList = class(TStringList);
  THandle = Longword;
{$ENDIF}
  TJvSimpleXml = class;
  TJvSimpleXmlInvalid = class(Exception);
  TJvSimpleXmlElem = class;
  TJvSimpleXmlElems = class;
  TJvSimpleXmlProps = class;
  TJvSimpleXmlElemComment = class;
  TJvSimpleXmlElemClassic = class;
  TJvSimpleXmlElemCData = class;
  TJvSimpleXmlElemText = class;
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

  TJvSimpleXmlProp = class(TObject)
  private
    FName: string;
    FValue: string;
    FParent: TJvSimpleXmlProps;
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
    property Parent: TJvSimpleXmlProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property Pointer: string read FPointer write FPointer;

    property Data: Pointer read FData write FData;
  end;

  TJvSimpleXmlProps = class(TObject)
  private
    FProperties: THashedStringList;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXmlProp;
  protected
    function GetItem(const Index: Integer): TJvSimpleXmlProp;
    procedure DoItemRename(var Value: TJvSimpleXmlProp; const Name: string);
    procedure Error(const S:string);
    procedure FmtError(const S:string;const Args:array of const);
  public
    destructor Destroy; override;
    function Add(const Name, Value: string): TJvSimpleXmlProp; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXmlProp; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXmlProp; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    property Item[const Index: Integer]: TJvSimpleXmlProp read GetItem; default;
    property ItemNamed[const Name: string]: TJvSimpleXmlProp read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXmlElemsProlog = class(TObject)
  private
    FElems: THashedStringList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TJvSimpleXmlElem;
  protected
    procedure Error(const S:string);
    procedure FmtError(const S:string;const Args:array of const);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil): string;
    procedure SaveToStream(const Stream: TStream; Parent: TJvSimpleXml = nil);
    property Item[const Index: Integer]: TJvSimpleXmlElem read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXmlElemCompare = function(Elems: TJvSimpleXmlElems; Index1, Index2: Integer): Integer of object;
  TJvSimpleXmlElems = class(TObject)
  private
    FParent: TJvSimpleXmlElem;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXmlElem;
  protected
    FElems: THashedStringList;
    FCompare: TJvSimpleXmlElemCompare;
    function GetItem(const Index: Integer): TJvSimpleXmlElem;
    procedure AddChild(const Value: TJvSimpleXmlElem);
    procedure AddChildFirst(const Value: TJvSimpleXmlElem);
    procedure DoItemRename(var Value: TJvSimpleXmlElem; const Name: string);
    procedure CreateElems;
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    function Add(const Name: string): TJvSimpleXmlElemClassic; overload;
    function Add(const Name, Value: string): TJvSimpleXmlElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXmlElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXmlElemClassic; overload;
    function Add(const Name: string; const Value: TStream): TJvSimpleXmlElemClassic; overload;
    function Add(Value: TJvSimpleXmlElem): TJvSimpleXmlElem; overload;
    function AddFirst(Value: TJvSimpleXmlElem): TJvSimpleXmlElem; overload;
    function AddFirst(const Name: string): TJvSimpleXmlElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TJvSimpleXmlElemComment;
    function AddCData(const Name: string; const Value: string): TJvSimpleXmlElemCDATA;
    function AddText(const Name: string; const Value: string): TJvSimpleXmlElemText;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure BinaryValue(const Name: string; const Stream: TStream);
    function LoadFromStream(const Stream: TStream; AParent: TJvSimpleXml = nil): string;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);
    procedure Sort;
    procedure CustomSort(AFunction: TJvSimpleXmlElemCompare);
    property Parent: TJvSimpleXmlElem read FParent write FParent;
    property Item[const Index: Integer]: TJvSimpleXmlElem read GetItem; default;
    property ItemNamed[const Name: string]: TJvSimpleXmlElem read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXmlElem = class(TObject)
  private
    FName: string;
    FParent: TJvSimpleXmlElem;
    FItems: TJvSimpleXmlElems;
    FProps: TJvSimpleXmlProps;
    FValue: string;
    FPointer: string;
    FData: Pointer;
  protected
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TJvSimpleXmlProps;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
    function GetItems: TJvSimpleXmlElems;
    procedure Error(const S:string);
    procedure FmtError(const S:string;const Args:array of const);
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    procedure Assign(Value: TJvSimpleXmlElem);
    procedure Clear; virtual;
    function SaveToString: string;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); virtual; abstract;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); virtual;
      abstract;
    procedure GetBinaryValue(const Stream: TStream);
    property Data: Pointer read FData write FData;
    function GetChildIndex(const AChild: TJvSimpleXmlElem): Integer;
  published
    property Name: string read FName write SetName;
    property Parent: TJvSimpleXmlElem read FParent write FParent;
    property Pointer: string read FPointer write FPointer;
    property ChildsCount: Integer read GetChildsCount;
    property Items: TJvSimpleXmlElems read GetItems;
    property Properties: TJvSimpleXmlProps read GetProps;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property Value: string read FValue write FValue;
  end;

  TJvSimpleXmlElemComment = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemClassic = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemCData = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemText = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemHeader = class(TJvSimpleXmlElem)
  private
    FStandalone: Boolean;
    FEncoding: string;
    FVersion: string;
  public
    constructor Create;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
    property Version: string read FVersion write FVersion;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;
  end;

  TJvSimpleXmlElemDocType = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXmlElemSheet = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil); override;
  end;

  TJvSimpleXml = class(TJvComponent)
  private
    FFileName: TFileName;
    FRoot: TJvSimpleXmlElemClassic;
    FOnTagParsed: TJvOnSimpleXmlParsed;
    FOnValue: TJvOnValueParsed;
    FOnLoadProg: TJvOnSimpleProgress;
    FOnSaveProg: TJvOnSimpleProgress;
    FProlog: TJvSimpleXmlElemsProlog;
    FSaveCount, FSaveCurrent: Integer;
  protected
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
    property Prolog: TJvSimpleXmlElemsProlog read FProlog write FProlog;
    property Root: TJvSimpleXmlElemClassic read FRoot write FRoot;
  published
    property FileName: TFileName read FFileName write SetFileName;
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
    Xml: TJvSimpleXmlElem;
    Reserved4: Longint;
  end;

procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXmlElem);
function XmlCreate(const AXml: TJvSimpleXmlElem): Variant; overload;
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

implementation

uses
  JvTypes;

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

//=== TJvSimpleXml ===========================================================

constructor TJvSimpleXml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoot := TJvSimpleXmlElemClassic.Create(nil);
  FProlog := TJvSimpleXmlElemsProlog.Create;
end;

destructor TJvSimpleXml.Destroy;
begin
  FRoot.Free;
  FProlog.Free;
  inherited Destroy;
end;

procedure TJvSimpleXml.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, APosition, ATotal);
end;

procedure TJvSimpleXml.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    Inc(FSaveCount);
    FOnSaveProg(Self, FSaveCurrent, FSaveCount);
  end;
end;

procedure TJvSimpleXml.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(Self, AName);
end;

procedure TJvSimpleXml.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(Self, AName, AValue);
end;

procedure TJvSimpleXml.LoadFromFile(const FileName: TFileName);
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

procedure TJvSimpleXml.LoadFromResourceName(Instance: THandle;
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

procedure TJvSimpleXml.LoadFromStream(const Stream: TStream);
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

procedure TJvSimpleXml.LoadFromString(const Value: string);
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

procedure TJvSimpleXml.SaveToFile(FileName: TFileName);
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

procedure TJvSimpleXml.SaveToStream(const Stream: TStream);
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

function TJvSimpleXml.SaveToString: string;
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

procedure TJvSimpleXml.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== TJvSimpleXmlElem =======================================================

procedure TJvSimpleXmlElem.Assign(Value: TJvSimpleXmlElem);
var
  Elems: TJvSimpleXmlElem;
  Elem: TJvSimpleXmlElem;
  I: Integer;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TJvSimpleXmlElem(Value);
  Name := Elems.Name;
  Self.Value := Elems.Value;
  for I := 0 to Elems.Properties.Count - 1 do
    Properties.Add(Elems.Properties[I].Name, Elems.Properties[I].Value);

  for I := 0 to Elems.Items.Count - 1 do
  begin
    Elem := Items.Add(Elems.Items[I].Name, Elems.Items[I].Value);
    Elem.Assign(TJvSimpleXmlElem(Elems.Items[I]));
  end;
end;

procedure TJvSimpleXmlElem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
  if FProps <> nil then
    FProps.Clear;
end;

constructor TJvSimpleXmlElem.Create(const AOwner: TJvSimpleXmlElem);
begin
  inherited Create;
  FName := '';
  FParent := TJvSimpleXmlElem(AOwner);
end;

destructor TJvSimpleXmlElem.Destroy;
begin
  Clear;
  FItems.Free;
  FProps.Free;
  inherited Destroy;
end;

procedure TJvSimpleXmlElem.Error(const S: string);
begin
  raise TJvSimpleXmlInvalid.Create(S);
end;

procedure TJvSimpleXmlElem.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S,Args));
end;

procedure TJvSimpleXmlElem.GetBinaryValue(const Stream: TStream);
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

function TJvSimpleXmlElem.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJvSimpleXmlElem.GetChildIndex(
  const AChild: TJvSimpleXmlElem): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.FElems.IndexOfObject(AChild);
end;

function TJvSimpleXmlElem.GetChildsCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if FItems <> nil then
    for I := 0 to FItems.Count - 1 do
      Result := Result + FItems[I].ChildsCount;
end;

function TJvSimpleXmlElem.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJvSimpleXmlElem.GetItems: TJvSimpleXmlElems;
begin
  if FItems = nil then
    FItems := TJvSimpleXmlElems.Create(Self);
  Result := FItems;
end;

function TJvSimpleXmlElem.GetProps: TJvSimpleXmlProps;
begin
  if FProps = nil then
    FProps := TJvSimpleXmlProps.Create();
  Result := FProps;
end;

function TJvSimpleXmlElem.SaveToString: string;
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

procedure TJvSimpleXmlElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJvSimpleXmlElem.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJvSimpleXmlElem.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.Items.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== TJvSimpleXmlElems ======================================================

function TJvSimpleXmlElems.Add(const Name: string): TJvSimpleXmlElemClassic;
begin
  Result := TJvSimpleXmlElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChild(Result);
end;

function TJvSimpleXmlElems.Add(const Name, Value: string): TJvSimpleXmlElemClassic;
begin
  Result := TJvSimpleXmlElemClassic.Create(Parent);
  Result.Name := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXmlElems.Add(const Name: string; const Value: Int64): TJvSimpleXmlElemClassic;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJvSimpleXmlElems.Add(Value: TJvSimpleXmlElem): TJvSimpleXmlElem;
begin
  if Value <> nil then
    AddChild(Value);
  Result := Value;
end;

function TJvSimpleXmlElems.Add(const Name: string;
  const Value: Boolean): TJvSimpleXmlElemClassic;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TJvSimpleXmlElems.Add(const Name: string;
  const Value: TStream): TJvSimpleXmlElemClassic;
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

procedure TJvSimpleXmlElems.AddChild(const Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.AddObject(Value.Name, Value);
end;

procedure TJvSimpleXmlElems.AddChildFirst(const Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.InsertObject(0, Value.Name, Value)
end;

function TJvSimpleXmlElems.AddFirst(const Name: string): TJvSimpleXmlElemClassic;
begin
  Result := TJvSimpleXmlElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChildFirst(Result);
end;

function TJvSimpleXmlElems.AddFirst(Value: TJvSimpleXmlElem): TJvSimpleXmlElem;
begin
  if Value <> nil then
    AddChildFirst(Value);
  Result := Value;
end;

function TJvSimpleXmlElems.AddComment(const Name,
  Value: string): TJvSimpleXmlElemComment;
begin
  Result := TJvSimpleXmlElemComment.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXmlElems.AddCData(const Name, Value: string): TJvSimpleXmlElemCDATA;
begin
  Result := TJvSimpleXmlElemCDATA.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJvSimpleXmlElems.AddText(const Name, Value: string): TJvSimpleXmlElemText;
begin
  Result := TJvSimpleXmlElemText.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

procedure TJvSimpleXmlElems.BinaryValue(const Name: string;
  const Stream: TStream);
var
  Elem: TJvSimpleXmlElem;
begin
  Elem := GetItemNamed(Name);
  if Elem <> nil then
    Elem.GetBinaryValue(Stream);
end;

function TJvSimpleXmlElems.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  Elem: TJvSimpleXmlElem;
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

procedure TJvSimpleXmlElems.Clear;
var
  I: Integer;
begin
  if FElems <> nil then
  begin
    for I := 0 to FElems.Count - 1 do
      // TJvSimpleXmlElem(FElems.Objects[I]).Clear; // (p3) not needed -called in Destroy
      TJvSimpleXmlElem(FElems.Objects[I]).Free;
    FElems.Clear;
  end;
end;

constructor TJvSimpleXmlElems.Create(const AOwner: TJvSimpleXmlElem);
begin
  FParent := AOwner;
end;

procedure TJvSimpleXmlElems.Delete(const Index: Integer);
begin
  if (FElems <> nil) and (Index >= 0) and (Index < FElems.Count) then
  begin
    TObject(FElems.Objects[Index]).Free;
    FElems.Delete(Index);
  end;
end;

procedure TJvSimpleXmlElems.CreateElems;
begin
  if FElems = nil then
    FElems := THashedStringList.Create;
end;

procedure TJvSimpleXmlElems.Delete(const Name: string);
begin
  if FElems <> nil then
    Delete(FElems.IndexOf(Name));
end;

destructor TJvSimpleXmlElems.Destroy;
begin
  Clear;
  if FElems <> nil then
    FElems.Free;
  inherited Destroy;
end;

procedure TJvSimpleXmlElems.DoItemRename(var Value: TJvSimpleXmlElem;
  const Name: string);
var
  I: Integer;
begin
  I := FElems.IndexOfObject(Value);
  if I <> -1 then
    FElems[I] := Name;
end;

function TJvSimpleXmlElems.GetCount: Integer;
begin
  if FElems = nil then
    Result := 0
  else
    Result := FElems.Count;
end;

function TJvSimpleXmlElems.GetItem(const Index: Integer): TJvSimpleXmlElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    Result := nil
  else
    Result := TJvSimpleXmlElem(FElems.Objects[Index]);
end;

function TJvSimpleXmlElems.GetItemNamed(const Name: string): TJvSimpleXmlElem;
var
  I: Integer;
begin
  Result := nil;
  if FElems <> nil then
  begin
    I := FElems.IndexOf(Name);
    if I <> -1 then
      Result := TJvSimpleXmlElem(FElems.Objects[I])
  end;
end;

function TJvSimpleXmlElems.IntValue(const Name: string; Default: Int64): Int64;
var
  Elem: TJvSimpleXmlElem;
begin
  Elem := GetItemNamed(Name);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.IntValue;
end;

function TJvSimpleXmlElems.LoadFromStream(const Stream: TStream; AParent: TJvSimpleXml): string;
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array[0..cBufferSize - 1] of Char;
  St: string;
  lElem: TJvSimpleXmlElem;
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
                lElem := TJvSimpleXmlElemText.Create(Parent);
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
                  lElem := TJvSimpleXmlElemClassic.Create(Parent);
                  St := St + lBuf[I];
                end;

              ' ', '>', ':': //This should be a classic tag
                begin
                  lElem := TJvSimpleXmlElemClassic.Create(Parent);
                  St := St + lBuf[I];
                end;
            else
              begin
                St := St + lBuf[I];
                if St = '<![CDATA[' then
                  lElem := TJvSimpleXmlElemCData.Create(Parent)
                else
                  if St = '<!--' then
                    lElem := TJvSimpleXmlElemComment.Create(Parent);
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

procedure TJvSimpleXmlElems.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, Level, Parent);
end;

function TJvSimpleXmlElems.Value(const Name: string; Default: string): string;
var
  Elem: TJvSimpleXmlElem;
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
    if TJvSimpleXmlElems(GSorts[i]).FElems = List then
    begin
      Result := TJvSimpleXmlElems(GSorts[i]).FCompare(TJvSimpleXmlElems(GSorts[i]), Index1, Index2);
      Exit;
    end;
end;

procedure TJvSimpleXmlElems.CustomSort(
  AFunction: TJvSimpleXmlElemCompare);
begin
  if FElems <> nil then
  begin
    GSorts.Add(self);
    FCompare := AFunction;
    FElems.CustomSort(SortItems);
    GSorts.Remove(self);
  end;
end;

procedure TJvSimpleXmlElems.Sort;
begin
  if FElems <> nil then
    FElems.Sort;
end;


//=== TJvSimpleXmlProps ======================================================

function TJvSimpleXmlProps.Add(const Name, Value: string): TJvSimpleXmlProp;
var
  Elem: TJvSimpleXmlProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TJvSimpleXmlProp.Create();
  FProperties.AddObject(Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TJvSimpleXmlProps.Add(const Name: string; const Value: Int64): TJvSimpleXmlProp;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJvSimpleXmlProps.Add(const Name: string; const Value: Boolean): TJvSimpleXmlProp;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TJvSimpleXmlProps.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  Prop: TJvSimpleXmlProp;
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

procedure TJvSimpleXmlProps.Clear;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    for I := 0 to FProperties.Count - 1 do
      TJvSimpleXmlProp(FProperties.Objects[I]).Free;
    FProperties.Clear;
  end;
end;

procedure TJvSimpleXmlProps.Delete(const Index: Integer);
begin
  if (FProperties <> nil) and (Index >= 0) and (Index < FProperties.Count) then
  begin
    TObject(FProperties.Objects[Index]).Free;
    FProperties.Delete(Index);
  end;
end;

procedure TJvSimpleXmlProps.Delete(const Name: string);
begin
  if FProperties <> nil then
    Delete(FProperties.IndexOf(Name));
end;

destructor TJvSimpleXmlProps.Destroy;
begin
  Clear;
  FProperties.Free;
  inherited Destroy;
end;

procedure TJvSimpleXmlProps.DoItemRename(var Value: TJvSimpleXmlProp;
  const Name: string);
var
  I: Integer;
begin
  if FProperties = nil then Exit;
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

procedure TJvSimpleXmlProps.Error(const S: string);
begin
  raise TJvSimpleXmlInvalid.Create(S);
end;

procedure TJvSimpleXmlProps.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S,Args));
end;

function TJvSimpleXmlProps.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

function TJvSimpleXmlProps.GetItem(const Index: Integer): TJvSimpleXmlProp;
begin
  if FProperties <> nil then
    Result := TJvSimpleXmlProp(FProperties.Objects[Index])
  else
    Result := nil;
end;

function TJvSimpleXmlProps.GetItemNamed(const Name: string): TJvSimpleXmlProp;
var
  I: Integer;
begin
  Result := nil;
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(Name);
    if I <> -1 then
      Result := TJvSimpleXmlProp(FProperties.Objects[I])
  end;
end;

function TJvSimpleXmlProps.IntValue(const Name: string; Default: Int64): Int64;
var
  Prop: TJvSimpleXmlProp;
begin
  Prop := GetItemNamed(Name);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.IntValue;
end;

procedure TJvSimpleXmlProps.LoadFromStream(const Stream: TStream);
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
              FmtError('Invalid XML Element: Unexpected character in properties declaration ("%s" found).',[lBuf[I]]);
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
            FmtError('Invalid XML Element: Unexpected character in properties declaration ("%s" found).',[lBuf[I]]);
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
            FmtError('Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but "%s"  found.',[lBuf[I]]);
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
            FmtError('Invalid XML Element: Unexpected character in properties declaration ("%s" found).',[lBuf[I]]);
          end;
      else
        Assert(False, 'Unexpected value for lPos');
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlProps.SaveToStream(const Stream: TStream);
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

function TJvSimpleXmlProps.Value(const Name: string; Default: string): string;
var
  Prop: TJvSimpleXmlProp;
begin
  Result := '';
  Prop := GetItemNamed(Name);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.Value;
end;

//=== TJvSimpleXmlProp =======================================================

function TJvSimpleXmlProp.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJvSimpleXmlProp.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJvSimpleXmlProp.SaveToString: string;
begin
  if Pointer <> '' then
    Result := Format(' %s:%s="%s"', [Pointer, Name, SimpleXmlEncode(Value)])
  else
    Result := Format(' %s="%s"', [Name, SimpleXmlEncode(Value)]);
end;

procedure TJvSimpleXmlProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJvSimpleXmlProp.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJvSimpleXmlProp.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== TJvSimpleXmlElemClassic ================================================

procedure TJvSimpleXmlElemClassic.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
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
            FmtError('Invalid XML Element: Expected beginning of tag but "%s" found.', [lBuf[I]]);
        -1:
          if lBuf[I] = '>' then
          begin
            Count := 0;
            Break;
          end
          else
            FmtError('Invalid XML Element: Expected end of tag but "%s" found.',[lBuf[I]]);
      else
        begin
          if lBuf[I] in [#9, #10, #13, ' ', '.'] then
          begin
            if lPos = 2 then
              Error('Invalid XML Element: malformed tag found (no valid name)');
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
                    FmtError('Invalid XML Element: Erroneous end of tag, expecting </%s> but </%s> found.',[lName,St]);
                  lStreamPos := Stream.Position;

                  //Set value if only one sub element
                  //This might reduce speed, but this is for compatibility issues
                  if (Items.Count = 1) and (Items[0] is TJvSimpleXmlElemText) then
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

procedure TJvSimpleXmlElemClassic.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  St := Level + '<' + Name;
  Stream.Write(St[1], Length(St));
  Properties.SaveToStream(Stream);

  if Items.Count = 0 then
  begin
    if Value = '' then
      St := '/>' + CrLf
    else
      St := '>' + SimpleXmlEncode(Value) + '</' + Name + '>' + CrLf;
    Stream.Write(St[1], Length(St));
  end
  else
  begin
    St := '>' + CrLf;
    Stream.Write(St[1], Length(St));
    Items.SaveToStream(Stream, Level + ' ', Parent);
    St := Level + '</' + Name + '>' + CrLf;
    Stream.Write(St[1], Length(St));
  end;
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemComment ================================================

procedure TJvSimpleXmlElemComment.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
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
            FmtError('Invalid Comment: expected "%s" but found "%s"',[CS_START_COMMENT[lPos],lBuf[I]]);
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
              Error('Invalid Comment: "--" not allowed inside comments');
            St := St + '--' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error('Invalid Comment: Unexpected end of data');

  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemComment.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
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

//=== TJvSimpleXmlElemCData ==================================================

procedure TJvSimpleXmlElemCData.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
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
            FmtError('Invalid CDATA: expected "%s" but found "%s"',[CS_START_CDATA[lPos],lBuf[I]]);
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
    Error('Invalid CDATA: Unexpected end of data');

  Value := St;
  Name := '';

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemCData.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
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

//=== TJvSimpleXmlElemText ===================================================

procedure TJvSimpleXmlElemText.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
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

procedure TJvSimpleXmlElemText.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
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

//=== TJvSimpleXmlElemHeader =================================================

constructor TJvSimpleXmlElemHeader.Create;
begin
  FVersion := '1.0';
  FEncoding := 'iso-8859-1';
  FStandalone := False;
end;

procedure TJvSimpleXmlElemHeader.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
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
            FmtError('Invalid Header: expected "%s" but found "%s"',[CS_START_HEADER[lPos],lBuf[I]]);
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
            FmtError('Invalid Header: expected "%s" but found "%s"',[CS_START_HEADER[lPos],lBuf[I]]);
        6: //?
          if lBuf[I] = CS_STOP_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError('Invalid Header: expected "%s" but found "%s"',[CS_STOP_HEADER[lPos],lBuf[I]]);
        7: //>
          if lBuf[I] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError('Invalid Header: expected "%s" but found "%s"',[CS_STOP_HEADER[lPos],lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error('Invalid Comment: Unexpected end of data');

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemHeader.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
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

//=== TJvSimpleXmlElemDocType ================================================

procedure TJvSimpleXmlElemDocType.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
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
            FmtError('Invalid Header: expected "%s" but found "%s"',[CS_START_DOCTYPE[lPos],lBuf[I]]);
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
    Error('Invalid Comment: Unexpected end of data');

  Name := '';
  Value := Trim(St);

  if Parent <> nil then
    Parent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemDocType.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
  St: string;
begin
  St := '<!DOCTYPE ' + Value + '>' + CrLf;
  Stream.Write(St[1], Length(St));
  if Parent <> nil then
    Parent.DoSaveProgress;
end;

//=== TJvSimpleXmlElemSheet ==================================================

procedure TJvSimpleXmlElemSheet.LoadFromStream(const Stream: TStream;
  Parent: TJvSimpleXml);
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
            FmtError('Invalid Stylesheet: expected "%s" but found "%s"',[CS_START_PI[lPos],lBuf[I]]);
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
            FmtError('Invalid Stylesheet: expected "%s" but found "%s"',[CS_START_PI[lPos],lBuf[I]]);
        17: //?
          if lBuf[I] = CS_STOP_PI[lPos] then
            Inc(lPos)
          else
            FmtError('Invalid Stylesheet: expected "%s" but found "%s"',[CS_STOP_PI[lPos],lBuf[I]]);
        18: //>
          if lBuf[I] = CS_STOP_PI[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError('Invalid Stylesheet: expected "%s" but found "%s"',[CS_STOP_PI[lPos],lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error('Invalid Stylesheet: Unexpected end of data');

  Name := '';

  Stream.Seek(lStreamPos, soFromBeginning);
end;

procedure TJvSimpleXmlElemSheet.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
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

//=== TJvSimpleXmlElemsProlog ================================================

constructor TJvSimpleXmlElemsProlog.Create;
begin
  inherited Create;
  FElems := THashedStringList.Create;
end;

destructor TJvSimpleXmlElemsProlog.Destroy;
begin
  Clear;
  FElems.Free;
  inherited Destroy;
end;

procedure TJvSimpleXmlElemsProlog.Clear;
var i: integer;
begin
  for i := 0 to FElems.Count - 1 do
    TJvSimpleXmlElem(FElems.Objects[i]).Free;
  FElems.Clear;
end;

function TJvSimpleXmlElemsProlog.GetCount: Integer;
begin
  Result := FElems.Count;
end;

function TJvSimpleXmlElemsProlog.GetItem(const Index: Integer): TJvSimpleXmlElem;
begin
  Result := TJvSimpleXmlElem(FElems.Objects[Index]);
end;

function TJvSimpleXmlElemsProlog.LoadFromStream(
  const Stream: TStream; Parent: TJvSimpleXml): string;
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
  lElem: TJvSimpleXmlElem;
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
              Error('Invalid Document: Unexpected text in file prolog.');
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
                lElem := TJvSimpleXmlElemComment.Create(nil)
              else
                if St = '<?xml-stylesheet' then
                  lElem := TJvSimpleXmlElemSheet.Create(nil)
                else
                  if St = '<?xml ' then
                    lElem := TJvSimpleXmlElemHeader.Create
                  else
                    if St = '<!DOCTYPE' then
                      lElem := TJvSimpleXmlElemDoctype.Create(nil)
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

procedure TJvSimpleXmlElemsProlog.SaveToStream(const Stream: TStream; Parent: TJvSimpleXml);
var
  I: Integer;
begin
  if Count = 0 then
    FElems.AddObject('', TJvSimpleXmlElemHeader.Create);
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

procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXmlElem);
begin
  TXmlVarData(ADest).VType := VarXml;
  TXmlVarData(ADest).Xml := AXml;
end;

function XmlCreate(const AXml: TJvSimpleXmlElem): Variant;
begin
  XmlCreateInto(Result, AXml);
end;

function XmlCreate: Variant;
begin
  XmlCreateInto(Result, TJvSimpleXmlElemClassic.Create(nil));
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
  lXml: TJvSimpleXmlElem;
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
  lXml: TJvSimpleXmlElem;
  lProp: TJvSimpleXmlProp;
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
  lXml: TJvSimpleXmlElem;
  lProp: TJvSimpleXmlProp;

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

procedure TJvSimpleXmlElemsProlog.Error(const S: string);
begin
  raise TJvSimpleXmlInvalid.Create(S);
end;

procedure TJvSimpleXmlElemsProlog.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S,Args));
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

