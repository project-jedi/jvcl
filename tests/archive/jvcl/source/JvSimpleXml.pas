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
  THashedStringlist = class(TStringlist);
  THandle = LongWord;
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
  TJvHashList = array [0..25] of PJvHashRecord;
  PJvHashList = ^TJvHashList;
  TJvHashRecord = packed record
    Count: Byte;
    case Kind:TJvHashKind of
      hkList: (List: PJvHashList);
      hkDirect: (FirstElem: PJvHashElem);
  end;

  TJvSimpleHashTable = class
  private
    FList: PJvHashRecord;
  public
    constructor Create;
    destructor Destroy;override;

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
  public
    destructor Destroy; override;
    function Add(const Name, Value: string): TJvSimpleXmlProp; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXmlProp; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXmlProp; overload;
    procedure Clear;virtual;
    procedure Delete(const Index: Integer);overload;
    procedure Delete(const Name: string);overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = true): Boolean;

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
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    
    function LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil):string;
    procedure SaveToStream(const Stream: TStream; Parent: TJvSimpleXml = nil);

    property Item[const Index: Integer]: TJvSimpleXmlElem read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TJvSimpleXmlElems = class(TObject)
  private
    FElems: THashedStringList;
    FParent: TJvSimpleXmlElem;
    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXmlElem;
  protected
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
    function AddComment(const Name: string; const Value: string):TJvSimpleXmlElemComment;
    function AddCData(const Name: string; const Value: string):TJvSimpleXmlElemCDATA;
    function AddText(const Name: string; const Value: string):TJvSimpleXmlElemText;
    procedure Clear;virtual;
    procedure Delete(const Index: Integer);overload;
    procedure Delete(const Name: string);overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = true): Boolean;
    procedure BinaryValue(const Name: string; const Stream: TStream);

    function LoadFromStream(const Stream: TStream; AParent: TJvSimpleXml = nil):string;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);

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
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    procedure Assign(Value: TJvSimpleXmlElem);
    procedure Clear;virtual;

    function SaveToString: string;
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);virtual;abstract;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);virtual;abstract;

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
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);override;
  end;

  TJvSimpleXmlElemClassic = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);override;
  end;

  TJvSimpleXmlElemCData = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);override;
  end;

  TJvSimpleXmlElemText = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);override;
  end;

  TJvSimpleXmlElemHeader = class(TJvSimpleXmlElem)
  private
    FStandalone: Boolean;
    FEncoding: string;
    FVersion: string;
  public
    constructor Create;

    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);override;
    property Version: string read FVersion write FVersion;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;
  end;

  TJvSimpleXmlElemDocType = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);override;
  end;

  TJvSimpleXmlElemSheet = class(TJvSimpleXmlElem)
  public
    procedure LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml = nil);override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; Parent: TJvSimpleXml = nil);override;
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

    property OnSaveProgress:TJvOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress:TJvOnSimpleProgress read FOnLoadProg write FOnLoadProg;
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
      const Name: string; const Arguments: TVarDataArray): Boolean;override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean;override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean;override;
  end;

  TXmlVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    Xml: TJvSimpleXmlElem;
    Reserved4: LongInt;
  end;

  procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXmlElem);
  function XmlCreate(const AXml: TJvSimpleXmlElem): Variant;overload;
  function XmlCreate: Variant;overload;
  function VarXml: TVarType;
  {$ENDIF}


resourcestring
  RS_INVALID_SimpleXml = 'Invalid XML file';
{$IFNDEF COMPILER6_UP}
  SInvalidBoolean = '''%s'' is not a valid boolean value';
  SLineBreak      = #13#10; 
{$ENDIF COMPILER6_UP}

implementation

uses
  JvTypes;

{$IFDEF COMPILER6_UP}
var
  XmlVariant: TXmlVariant = nil;
{$ENDIF}
  
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
  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
      if AnsiSameText(S, aArray[I]) then
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
  cSimpleBoolStrs: array[boolean] of string = ('0', '-1');
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

{*************************************************}

function SimpleXmlEncode(const Value: string): string;
var
  i: Integer;
  lDiff: Boolean;
begin
  //http://www.cs.tut.fi/~jkorpela/latin1/3.html#60
  result := Value;
  lDiff := false;
  for i := 1 to Length(Value) do
    if Value[i] in ['<','>','&','"',''''] then
    begin
      if not lDiff then
      begin
        lDiff := true;
        result := Copy(Value,1,i-1);
      end;
      result := result + '&#' + IntToStr(Ord(Value[i])) + ';';
    end
    else
      if lDiff then
        result := result + Value[i];
end;
{*************************************************}

procedure SimpleXmlDecode(var Value: string; TrimMultiple: Boolean = true);
var
 i, j, k, l: Integer;
 st: string;
begin
  st := '';
  j := -1;
  k := 1;
  for i := 1 to Length(Value) do
    case Value[i] of
      ' ',#10,#13:
        if (not TrimMultiple) or ((k=1) or not (Value[k-1] in [' ',#10,#13])) then
        begin
          if j>0 then
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
            for l:=1 to Length(st) do
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
          for l:=1 to Length(st) do
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
          for l:=1 to Length(st) do
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
                else if j>=0 then
                begin
                  Value[k] := '&';
                  inc(k);
                  for l:=1 to Length(st) do
                  begin
                    Value[k] := st[l];
                    inc(k);
                  end;
                  Value[k] := ';';
                  inc(k);
                end
                else
                begin
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
            for l:=1 to Length(st) do
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
    for l:=1 to Length(st) do
    begin
      Value[k] := st[l];
      inc(k);
    end;
  end;
  SetLength(Value,k-1);
end;
{*************************************************}

{*************************************************}

constructor TJvSimpleXml.Create(AOwner: TComponent);
begin
  inherited;
  FRoot := TJvSimpleXmlElemClassic.Create(nil);
  FProlog := TJvSimpleXmlElemsProlog.Create;
end;
{*************************************************}

destructor TJvSimpleXml.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited;
end;
{*************************************************}

procedure TJvSimpleXml.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(self,APosition,ATotal);
end;
{*************************************************}

procedure TJvSimpleXml.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    inc(FSaveCount);
    FOnSaveProg(self, FSaveCurrent, FSaveCount);
  end;
end;
{*************************************************}

procedure TJvSimpleXml.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(self, AName);
end;
{*************************************************}

procedure TJvSimpleXml.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(self, AName, AValue);
end;
{*************************************************}

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
{*************************************************}

procedure TJvSimpleXml.LoadFromResourceName(Instance: THandle;
  const ResName: string);
const
  RT_RCDATA       = PChar(10);
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
{*************************************************}

procedure TJvSimpleXml.LoadFromStream(const Stream: TStream);
begin
  FRoot.Clear;
  FProlog.Clear;
  if Assigned(FOnLoadProg) then
  begin
    FOnLoadProg(self,Stream.Position,Stream.Size);
    //Read doctype and so on
    FProlog.LoadFromStream(Stream,self);
    //Read elements
    FRoot.LoadFromStream(Stream,self);
    FOnLoadProg(self,Stream.Position,Stream.Size);
  end
  else
  begin
    if Assigned(FOnTagParsed) or Assigned(FOnValue) then
    begin
      FProlog.LoadFromStream(Stream,self);
      FRoot.LoadFromStream(Stream,self);
    end
    else
    begin
      FProlog.LoadFromStream(Stream);
      FRoot.LoadFromStream(Stream);
    end;
  end;
end;
{*************************************************}

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
{*************************************************}

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
{*************************************************}

procedure TJvSimpleXml.SaveToStream(const Stream: TStream);
var
 lCount: Integer;
begin
  if Assigned(FOnSaveProg) then
  begin
    lCount := Root.ChildsCount + Prolog.Count;
    FSaveCount := lCount;
    FSaveCurrent := 0;
    FOnSaveProg(self,0,lCount);
    Prolog.SaveToStream(Stream,self);
    Root.SaveToStream(Stream,'',self);
    FOnSaveProg(self,lCount,lCount);
  end
  else
  begin
    Prolog.SaveToStream(Stream);
    Root.SaveToStream(Stream);
  end;
end;
{*************************************************}

function TJvSimpleXml.SaveToString: string;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create('');
  try
    SaveToStream(LStream);
    result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;
{*************************************************}

procedure TJvSimpleXml.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;
{*************************************************}

{ TJvSimpleXmlElem }

{*************************************************}

procedure TJvSimpleXmlElem.Assign(Value: TJvSimpleXmlElem);
var
  Elems: TJvSimpleXmlElem;
  Elem: TJvSimpleXmlElem;
  i: Integer;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TJvSimpleXmlElem(Value);
  Name := Elems.Name;
  self.Value := Elems.Value;
  for i := 0 to Elems.Properties.Count - 1 do
    Properties.Add(Elems.Properties[i].Name, Elems.Properties[i].Value);

  for i := 0 to Elems.Items.Count - 1 do
  begin
    Elem := Items.Add(Elems.Items[i].Name, Elems.Items[i].Value);
    Elem.Assign(TJvSimpleXmlElem(Elems.Items[i]));
  end;
end;
{*************************************************}

procedure TJvSimpleXmlElem.Clear;
begin
  if FItems<>nil then
    FItems.Clear;
  if FProps<>nil then
    Properties.Clear;
end;
{*************************************************}

constructor TJvSimpleXmlElem.Create(const AOwner: TJvSimpleXmlElem);
begin
  FName := '';
  FParent := TJvSimpleXmlElem(AOwner);
end;
{*************************************************}

destructor TJvSimpleXmlElem.Destroy;
begin
  Clear;
  if FItems<>nil then
    FItems.Free;
  if FProps<>nil then
    FProps.Free;
  inherited;
end;
{*************************************************}

procedure TJvSimpleXmlElem.GetBinaryValue(const Stream: TStream);
var
 i,j: Integer;
 st: string;
 buf: array [0..1024] of byte;
begin
  i := 1;
  j := 0;
  while (i<Length(Value)) do
  begin
    st := '$'+Value[i]+Value[i+1];
    if j=1024 then //Buffered write to speed up a little the process
    begin
      Stream.Write(buf,j);
      j := 0;
    end;
    buf[j] := StrToIntDef(st,0);
    inc(j);
    inc(i,2);
  end;
  Stream.Write(buf,j);
end;
{*************************************************}

function TJvSimpleXmlElem.GetBoolValue: Boolean;
begin
  result := StrToBoolDef(Value,false);
end;
{*************************************************}

function TJvSimpleXmlElem.GetChildIndex(
  const AChild: TJvSimpleXmlElem): Integer;
begin
  if (FItems=nil) then
    result := -1
  else
    result := FItems.FElems.IndexOfObject(AChild);
end;
{*************************************************}

function TJvSimpleXmlElem.GetChildsCount: Integer;
var
 i: Integer;
begin
  result := 1;
  if FItems<>nil then
    for i:=0 to FItems.Count-1 do
      result := result + FItems[i].ChildsCount;
end;
{*************************************************}

function TJvSimpleXmlElem.GetIntValue: Int64;
begin
  result := StrToInt64Def(Value, -1);
end;
{*************************************************}

function TJvSimpleXmlElem.GetItems: TJvSimpleXmlElems;
begin
  if FItems=nil then
    FItems := TJvSimpleXmlElems.Create(self);
  result := FItems;
end;
{*************************************************}

function TJvSimpleXmlElem.GetProps: TJvSimpleXmlProps;
begin
  if FProps=nil then
    FProps := TJvSimpleXmlProps.Create();
  result := FProps;
end;

{*************************************************}

function TJvSimpleXmlElem.SaveToString: string;
var
 lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  try
    SaveToStream(lStream);
    result := lStream.DataString;
  finally
    lStream.Free;
  end;
end;
{*************************************************}

procedure TJvSimpleXmlElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;
{*************************************************}

procedure TJvSimpleXmlElem.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;
{*************************************************}

procedure TJvSimpleXmlElem.SetName(const Value: string);
begin
  if (Value<>FName) and (Value<>'') then
  begin
    if (Parent<>nil) and (FName<>'') then
      Parent.Items.DoItemRename(self,Value);
    FName := Value;
  end;
end;
{*************************************************}

{ TJvSimpleXmlElems }

{*************************************************}

function TJvSimpleXmlElems.Add(const Name: string): TJvSimpleXmlElemClassic;
begin
  result := TJvSimpleXmlElemClassic.Create(Parent);
  result.FName := Name; //Directly set parent to avoid notification 
  AddChild(result);
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name, Value: string): TJvSimpleXmlElemClassic;
begin
  result := TJvSimpleXmlElemClassic.Create(Parent);
  result.Name := Name;
  result.Value := Value;
  AddChild(result);
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name: string; const Value: Int64): TJvSimpleXmlElemClassic;
begin
  result := Add(Name, IntToStr(Value));
end;
{*************************************************}

function TJvSimpleXmlElems.Add(Value: TJvSimpleXmlElem): TJvSimpleXmlElem;
begin
  if Value<>nil then
    AddChild(Value);
  result := Value;
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name: string;
  const Value: Boolean): TJvSimpleXmlElemClassic;
begin
  result := Add(Name, BoolToStr(Value));
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name: string;
  const Value: TStream): TJvSimpleXmlElemClassic;
var
 stream: TStringStream;
 buf: array [0..1024] of byte;
 st: string;
 i,count: Integer;
begin
  stream := TStringStream.Create('');
  repeat
    st := '';
    count := Value.Read(buf,SizeOf(buf));
    for i:=0 to count-1 do
      st := st+IntToHex(buf[i],2);
    stream.WriteString(st);
  until count = 0;
  result := Add(Name,stream.DataString);
  stream.Free;
end;
{*************************************************}

procedure TJvSimpleXmlElems.AddChild(const Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.AddObject(Value.Name,Value);
end;
{*************************************************}

procedure TJvSimpleXmlElems.AddChildFirst(const Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.InsertObject(0,Value.Name,Value)
end;
{*************************************************}

function TJvSimpleXmlElems.AddFirst(const Name: string): TJvSimpleXmlElemClassic;
begin
  result := TJvSimpleXmlElemClassic.Create(Parent);
  result.FName := Name; //Directly set parent to avoid notification
  AddChildFirst(result);
end;

{*************************************************}
function TJvSimpleXmlElems.AddFirst(
  Value: TJvSimpleXmlElem): TJvSimpleXmlElem;
begin
  if Value<>nil then
    AddChildFirst(Value);
  result := Value;
end;

{*************************************************}
function TJvSimpleXmlElems.AddComment(const Name,
  Value: string): TJvSimpleXmlElemComment;
begin
  result := TJvSimpleXmlElemComment.Create(Parent);
  result.FName := Name;
  result.Value := Value;
  AddChild(result);
end;

{*************************************************}
function TJvSimpleXmlElems.AddCData(const Name,
  Value: string): TJvSimpleXmlElemCDATA;
begin
  result := TJvSimpleXmlElemCDATA.Create(Parent);
  result.FName := Name;
  result.Value := Value;
  AddChild(result);
end;

{*************************************************}
function TJvSimpleXmlElems.AddText(const Name,
  Value: string): TJvSimpleXmlElemText;
begin
  result := TJvSimpleXmlElemText.Create(Parent);
  result.FName := Name;
  result.Value := Value;
  AddChild(result);
end;
{*************************************************}

procedure TJvSimpleXmlElems.BinaryValue(const Name: string;
  const Stream: TStream);
var
  elem: TJvSimpleXmlElem;
begin
  elem := GetItemNamed(Name);
  if elem <> nil then
    elem.GetBinaryValue(Stream);
end;
{*************************************************}

function TJvSimpleXmlElems.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  elem: TJvSimpleXmlElem;
begin
  try
    elem := GetItemNamed(Name);
    if (elem = nil) or (elem.Value='') then
      result := Default
    else
      result := elem.BoolValue;
  except
    result := Default;
  end;
end;
{*************************************************}

procedure TJvSimpleXmlElems.Clear;
var
 i: Integer;
begin
  if FElems=nil then
    Exit;
  for i:=0 to FElems.Count-1 do
  try
    TJvSimpleXmlElem(FElems.Objects[i]).Clear;
    TJvSimpleXmlElem(FElems.Objects[i]).Free;
  except
  end;
  FElems.Clear;
end;
{*************************************************}

constructor TJvSimpleXmlElems.Create(const AOwner: TJvSimpleXmlElem);
begin
  FParent := AOwner;
end;
{*************************************************}

procedure TJvSimpleXmlElems.Delete(const Index: Integer);
begin
  if (FElems<>nil) and (Index>=0) and (Index<FElems.Count) then
    FElems.Delete(Index);
end;
{*************************************************}

procedure TJvSimpleXmlElems.CreateElems;
begin
  if FElems=nil then
    FElems := THashedStringList.Create;
end;

{*************************************************}

procedure TJvSimpleXmlElems.Delete(const Name: string);
begin
  if FElems<>nil then
    Delete(FElems.IndexOf(Name));
end;
{*************************************************}

destructor TJvSimpleXmlElems.Destroy;
begin
  Clear;
  if FElems<>nil then
    FElems.Free;
  inherited;
end;
{*************************************************}

procedure TJvSimpleXmlElems.DoItemRename(var Value: TJvSimpleXmlElem;
  const Name: string);
var
 i: Integer;
begin
  i := FElems.IndexOfObject(Value);
  if i<>-1 then
    FElems[i] := Name;
end;
{*************************************************}

function TJvSimpleXmlElems.GetCount: Integer;
begin
  if FElems = nil then
    result := 0
  else
    result := FElems.Count;
end;
{*************************************************}

function TJvSimpleXmlElems.GetItem(const Index: Integer): TJvSimpleXmlElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    result := nil
  else
    result := TJvSimpleXmlElem(FElems.Objects[Index]);
end;
{*************************************************}

function TJvSimpleXmlElems.GetItemNamed(const Name: string): TJvSimpleXmlElem;
var
  i: Integer;
begin
  result := nil;
  if FElems<>nil then
  begin
    i := FElems.IndexOf(Name);
    if i<>-1 then
      result := TJvSimpleXmlElem(FElems.Objects[i])
  end;
end;
{*************************************************}

function TJvSimpleXmlElems.IntValue(const Name: string; Default: Int64): Int64;
var
  elem: TJvSimpleXmlElem;
begin
  elem := GetItemNamed(Name);
  if elem = nil then
    result := Default
  else
    result := elem.IntValue;
end;


{*************************************************}
function TJvSimpleXmlElems.LoadFromStream(const Stream: TStream; AParent: TJvSimpleXml): string;
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 st: string;
 lElem: TJvSimpleXmlElem;
begin
  lStreamPos := Stream.Position;
  result := '';
  st := '';
  lPos := 0;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if AParent<>nil then
      AParent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces
          begin
            case lBuf[i] of
              ' ',#9,#13,#10:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  st := lBuf[i];
                end;
              else
                begin
                  //This is a text
                  lElem := TJvSimpleXmlElemText.Create(Parent);
                  Stream.Seek(lStreamPos-1,soFromBeginning);
                  lElem.LoadFromStream(Stream);
                  lStreamPos := Stream.Position;
                  CreateElems;
                  FElems.AddObject(lElem.Name,lElem);
                  Break;
                end;
            end;
          end;

        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            case lBuf[i] of
              '/':
                if st = '<' then
                begin
                  lPos := 2;
                  st := '';
                end
                else
                begin
                  lElem := TJvSimpleXmlElemClassic.Create(Parent);
                  st := st + lBuf[i];
                end;

              ' ', '>', ':': //This should be a classic tag
                begin
                  lElem := TJvSimpleXmlElemClassic.Create(Parent);
                  st := st + lBuf[i];
                end;
              else
                begin
                  st := st + lBuf[i];
                  if st='<![CDATA[' then
                    lElem := TJvSimpleXmlElemCData.Create(Parent)
                  else if st='<!--' then
                    lElem := TJvSimpleXmlElemComment.Create(Parent);
                  //<?
                end;
            end;

            if lElem<>nil then
            begin
              CreateElems;
              Stream.Seek(lStreamPos - (Length(st)),soFromBeginning);
              lElem.LoadFromStream(Stream);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name,lElem);
              st := '';
              lPos := 0;
              Break;
            end;
          end;

        2: //This is an end tag
          if lBuf[i] = '>' then
          begin
            result := st;
            Count := 0;
            Break;
          end
          else
            st := st + lBuf[i];
      end;
    end;
  until count = 0;

  Stream.Seek(lStreamPos,soFromBeginning);
end;

{*************************************************}
procedure TJvSimpleXmlElems.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
 i: Integer;
begin
  for i:=0 to Count-1 do
    Item[i].SaveToStream(Stream, Level, Parent);
end;
{*************************************************}

function TJvSimpleXmlElems.Value(const Name: string; Default: string): string;
var
  elem: TJvSimpleXmlElem;
begin
  result := '';
  elem := GetItemNamed(Name);
  if elem = nil then
    result := Default
  else
    result := elem.Value;
end;
{*************************************************}

{ TJvSimpleXmlProps }

{*************************************************}

function TJvSimpleXmlProps.Add(const Name, Value: string): TJvSimpleXmlProp;
var
  Elem: TJvSimpleXmlProp;
begin
  if FProperties=nil then
    FProperties := THashedStringList.Create;
  Elem := TJvSimpleXmlProp.Create();
  FProperties.AddObject(Name,Elem);
  Elem.FName := Name; //Avoid notification 
  Elem.Value := Value;
  Elem.Parent := self;
  result := Elem;
end;
{*************************************************}

function TJvSimpleXmlProps.Add(const Name: string; const Value: Int64): TJvSimpleXmlProp;
begin
  result := Add(Name, IntToStr(Value));
end;
{*************************************************}

function TJvSimpleXmlProps.Add(const Name: string; const Value: Boolean): TJvSimpleXmlProp;
begin
  result := Add(Name, BoolToStr(Value));
end;
{*************************************************}

function TJvSimpleXmlProps.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  prop: TJvSimpleXmlProp;
begin
  try
    prop := GetItemNamed(Name);
    if (prop = nil) or (prop.Value = '') then
      result := Default
    else
      result := prop.BoolValue;
  except
    result := Default;
  end;
end;
{*************************************************}

procedure TJvSimpleXmlProps.Clear;
var
 i: Integer;
begin
  if FProperties=nil then
    Exit;
  for i:=0 to FProperties.Count-1 do
  try
    TJvSimpleXmlProp(FProperties.Objects[i]).Free;
  except
  end;
  FProperties.Clear;
end;
{*************************************************}

procedure TJvSimpleXmlProps.Delete(const Index: Integer);
begin
  if (FProperties<>nil) and (Index>=0) and (Index<FProperties.Count) then
    FProperties.Delete(Index);
end;
{*************************************************}

procedure TJvSimpleXmlProps.Delete(const Name: string);
begin
  if FProperties<>nil then
    Delete(FProperties.IndexOf(Name));
end;
{*************************************************}

destructor TJvSimpleXmlProps.Destroy;
begin
  Clear;
  if FProperties<>nil then
    FProperties.Free;
  inherited;
end;
{*************************************************}

procedure TJvSimpleXmlProps.DoItemRename(var Value: TJvSimpleXmlProp;
  const Name: string);
var
 i: Integer;
begin
  i := FProperties.IndexOfObject(Value);
  if i<>-1 then
    FProperties[i] := Name;
end;
{*************************************************}

function TJvSimpleXmlProps.GetCount: Integer;
begin
  if FProperties=nil then
    result := 0
  else
    result := FProperties.Count;
end;
{*************************************************}

function TJvSimpleXmlProps.GetItem(const Index: Integer): TJvSimpleXmlProp;
begin
  if FProperties<>nil then
    result := TJvSimpleXmlProp(FProperties.Objects[Index])
  else
    result := nil;
end;
{*************************************************}

function TJvSimpleXmlProps.GetItemNamed(const Name: string): TJvSimpleXmlProp;
var
  i: Integer;
begin
  result := nil;
  if FProperties<>nil then
  begin
    i := FProperties.IndexOf(Name);
    if i<>-1 then
      result := TJvSimpleXmlProp(FProperties.Objects[i])
  end;
end;
{*************************************************}

function TJvSimpleXmlProps.IntValue(const Name: string; Default: Int64): Int64;
var
  prop: TJvSimpleXmlProp;
begin
  prop := GetItemNamed(Name);
  if prop = nil then
    result := Default
  else
    result := prop.IntValue;
end;
{*************************************************}

procedure TJvSimpleXmlProps.LoadFromStream(const Stream: TStream);
//<element prop="foo" prop='bar' foo:bar="beuh"/>
//Stop on / or ? or >
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 lName, lValue, lPointer: string;
 lPropStart: char;
begin
  lStreamPos := Stream.Position;
  lValue := '';
  lPointer := '';
  lName := '';
  lPropStart := ' ';
  lPos := 0;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        0: //We are waiting for a property
          begin
            case lBuf[i] of
              ' ', #9, #10, #13:
                begin
                end;
              'a'..'z','A'..'Z','0'..'9','-','_':
                begin
                  lName := lBuf[i];
                  lPos := 1;
                end;
              '/', '>', '?':
                begin
                  dec(lStreamPos);
                  count := 0;
                  Break;
                end;
              else
                raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in properties declaration ('+lBuf[i]+' found).');
            end;
          end;

        1: //We are reading a property name
          case lBuf[i] of
            'a'..'z','A'..'Z','0'..'9','-','_':
              lName := lName + lBuf[i];
            ':':
              begin
                lPointer := lName;
                lName := '';
              end;
            '=':
              lPos := 2;
            else
              raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in properties declaration ('+lBuf[i]+' found).');
          end;

        2: //We are going to start a property content
          if lBuf[i] in ['''','"'] then
          begin
            lPropStart := lBuf[i];
            lValue := '';
            lPos := 3;
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but '+lBuf[i]+' found.');

        3: //We are reading a property
          if lBuf[i] = lPropStart then
          begin
            SimpleXmlDecode(lValue,false);
            with Add(lName, lValue) do
              Pointer := lPointer;
            lPos := 0;
          end
          else
            lValue := lValue + lBuf[i];
      end;
    end;
  until count=0;

  Stream.Seek(lStreamPos,soFromBeginning);
end;

{*************************************************}
procedure TJvSimpleXmlProps.SaveToStream(const Stream: TStream);
var
 st: string;
 i: Integer;
begin
  st := '';
  for i:=0 to Count-1 do
    st := st + Item[i].SaveToString;
  if st<>'' then
    Stream.Write(st[1],Length(st));
end;
{*************************************************}

function TJvSimpleXmlProps.Value(const Name: string; Default: string): string;
var
  prop: TJvSimpleXmlProp;
begin
  result := '';
  prop := GetItemNamed(Name);
  if prop = nil then
    result := Default
  else
    result := prop.Value;
end;
{*************************************************}

{ TJvSimpleXmlProp }

{*************************************************}

function TJvSimpleXmlProp.GetBoolValue: Boolean;
begin
  result := StrToBoolDef(Value,false);
end;
{*************************************************}

function TJvSimpleXmlProp.GetIntValue: Int64;
begin
  result := StrToInt64Def(Value, -1);
end;
{*************************************************}

function TJvSimpleXmlProp.SaveToString: string;
begin
  if Pointer<>'' then
    result := Format(' %s:%s="%s"',[Pointer,Name,SimpleXmlEncode(Value)])
  else
    result := Format(' %s="%s"',[Name,SimpleXmlEncode(Value)]);
end;
{*************************************************}

procedure TJvSimpleXmlProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;
{*************************************************}

procedure TJvSimpleXmlProp.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;
{*************************************************}

procedure TJvSimpleXmlProp.SetName(const Value: string);
begin
  if (Value<>FName) and (Value<>'') then
  begin
    if (Parent<>nil) and (FName<>'') then
      Parent.DoItemRename(self,Value);
    FName := Value;
  end;
end;
{*************************************************}

{ TJvSimpleXmlElemClassic }

{*************************************************}
procedure TJvSimpleXmlElemClassic.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<element prop="foo" prop='bar'/>
//<element prop="foo" prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element prop="foo" prop='bar'>foor<b>beuh</b>bar</element>
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 st, lName, lValue, lPointer: string;
begin
  lStreamPos := Stream.Position;
  st := '';
  lValue := '';
  lPointer := '';
  lPos := 1;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        1:
          if lBuf[i] = '<' then
            lPos := 2
          else
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Expected beginning of tag but '+lBuf[i]+' found.');

        -1:
          if lBuf[i] = '>' then
          begin
            count := 0;
            Break;
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Expected end of tag but '+lBuf[i]+' found.');

        else
          begin
            if lBuf[i] in [' ',#9,#10,#13] then
            begin
              if lPos=2 then
                raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Malformed tag found (no valid name)');
              Stream.Seek(lStreamPos, soFromBeginning);
              Properties.LoadFromStream(Stream);
              lStreamPos := Stream.Position;
              Break; //Re read buffer
            end
            else
            begin
              case lBuf[i] of
                '>':
                  begin
                    lName := st;
                    
                    //Load elements
                    Stream.Seek(lStreamPos, soFromBeginning);
                    st := Items.LoadFromStream(Stream, Parent);
                    if lName <> st then
                      raise TJvSimpleXmlInvalid.Create('Invalid XML Element: Erroneous end of tag, expecting </'+lName+'> but </'+st+'> found.');
                    lStreamPos := Stream.Position;

                    //Set value if only one sub element
                    //This might reduce speed, but this is for compatibility issues
                    if (Items.Count = 1) and (Items[0] is TJvSimpleXmlElemText) then
                    begin
                      lValue := Items[0].Value;
                      Items.Clear;
                    end;

                    count := 0;
                    Break;
                  end;
                  
                '/':
                  begin
                    lName := st;
                    lPos := -1;
                  end;

                ':':
                  begin
                    lPointer := st;
                    st := '';
                  end;

                else
                  begin
                    st := st + lBuf[i];
                    inc(lPos);
                  end;
              end;
            end;
          end;
        end;
    end;
  until count=0;

  Name := lName;
  SimpleXmlDecode(lValue);
  Value := lValue;
  Pointer := lPointer;

  if Parent<>nil then
  begin
    Parent.DoTagParsed(lName);
    Parent.DoValueParsed(lName,lValue);
  end;

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemClassic.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
 st: string;
begin
  st := Level + '<' + Name;
  Stream.Write(st[1], Length(st));
  Properties.SaveToStream(Stream);

  if Items.Count = 0 then
  begin
    if Value='' then
      st := '/>' + sLineBreak
    else
      st := '>' + SimpleXmlEncode(Value) + '</' + Name + '>' + sLineBreak;
    Stream.Write(st[1], Length(st));
  end
  else
  begin
    st := '>' + sLineBreak;
    Stream.Write(st[1], Length(st));
    Items.SaveToStream(Stream, Level + ' ', Parent);
    st := Level + '</' + Name + '>' + sLineBreak;
    Stream.Write(st[1], Length(st));
  end;
  if Parent<>nil then
    Parent.DoSaveProgress;
end;
{*************************************************}

{ TJvSimpleXmlElemComment }

{*************************************************}
procedure TJvSimpleXmlElemComment.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<!-- declarations for <head> & <body> -->
const
  CS_START_COMMENT   =   '<!--';
  CS_STOP_COMMENT    =   '    -->';
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 st: string;
 lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  st := '';
  lPos := 1;
  lOk := false;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        1..4: //<!--
          if lBuf[i] = CS_START_COMMENT[lPos] then
            inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Comment: Expected ' + CS_START_COMMENT[lPos] +
              ' Found ' + lBuf[i]);

        5:
          if lBuf[i] = CS_STOP_COMMENT[lPos] then
            inc(lPos)
          else
            st := st + lBuf[i];

        6: //-
          if lBuf[i] = CS_STOP_COMMENT[lPos] then
            inc(lPos)
          else
          begin
            st := st + '-' + lBuf[i];
            dec(lPos);
          end;

        7: //>
          if lBuf[i] = CS_STOP_COMMENT[lPos] then
          begin
            Count := 0; //End repeat
            lOk := true; 
            Break; //End if
          end
          else
          begin
            st := st + '--' + lBuf[i];
            dec(lPos,2);
          end;
      end;
    end;
  until count=0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Value := st;
  Name := '';

  if (Parent<>nil) then
    Parent.DoValueParsed('',st);

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemComment.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
 st: string;
begin
  st := Level + '<!--';
  Stream.Write(st[1], Length(st));
  if (Value<>'') then
    Stream.Write(Value[1], Length(Value));
  st := '-->' + sLineBreak;
  Stream.Write(st[1], Length(st));
  if Parent<>nil then
    Parent.DoSaveProgress;
end;
{*************************************************}

{ TJvSimpleXmlElemCData }

{*************************************************}
procedure TJvSimpleXmlElemCData.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
const
  CS_START_CDATA   =   '<![CDATA[';
  CS_STOP_CDATA    =   '         ]]>';
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 st: string;
 lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  st := '';
  lPos := 1;
  lOk := false;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        1..9: //<![CDATA[
          if lBuf[i] = CS_START_CDATA[lPos] then
            inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid CDATA: Expected ' + CS_START_CDATA[lPos] +
              ' Found ' + lBuf[i]);

        10:
          if lBuf[i] = CS_STOP_CDATA[lPos] then
            inc(lPos)
          else
            st := st + lBuf[i];

        11: //-
          if lBuf[i] = CS_STOP_CDATA[lPos] then
            inc(lPos)
          else
          begin
            st := st + ']' + lBuf[i];
            dec(lPos);
          end;

        12: //>
          if lBuf[i] = CS_STOP_CDATA[lPos] then
          begin
            Count := 0; //End repeat
            lOk := true;
            Break; //End if
          end
          else
          begin
            st := st + ']]' + lBuf[i];
            dec(lPos,2);
          end;
      end;
    end;
  until count=0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid CDATA: Unexpected end of data');

  Value := st;
  Name := '';

  if Parent<>nil then
    Parent.DoValueParsed('',st);

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemCData.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
 st: string;
begin
  st := Level + '<![CDATA[';
  Stream.Write(st[1], Length(st));
  if (Value<>'') then
    Stream.Write(Value[1], Length(Value));
  st := ']]>' + sLineBreak;
  Stream.Write(st[1], Length(st));
  if Parent<>nil then
    Parent.DoSaveProgress;
end;
{*************************************************}

{ TJvSimpleXmlElemText }

{*************************************************}
procedure TJvSimpleXmlElemText.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 st: string;
begin
  lStreamPos := Stream.Position;
  st := '';
  lPos := 0;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lBuf[i] of
        '<':
          begin
            //Quit text
            dec(lStreamPos);
            count := 0;
            Break;
          end;
        ' ':
          if lPos=0 then
          begin
            inc(lPos);
            st := st + ' ';
          end;
        else
          begin
            lPos := 0;
            st := st + lBuf[i];
          end;
      end;
    end;
  until count=0;

  SimpleXmlDecode(st);
  Value := st;
  Name := '';

  if (Parent<>nil) then
    Parent.DoValueParsed('',st);

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemText.SaveToStream(const Stream: TStream; const Level: string; Parent: TJvSimpleXml);
var
 st: string;
begin
  if (Value<>'') then
  begin
    st := Level + SimpleXmlEncode(Value) + sLineBreak;
    Stream.Write(st[1], Length(st));
  end;
  if Parent<>nil then
    Parent.DoSaveProgress;
end;
{*************************************************}


{ TJvSimpleXmlElemHeader }

{*************************************************}
constructor TJvSimpleXmlElemHeader.Create;
begin
  FVersion := '1.0';
  FEncoding := 'iso-8859-1';
  FStandalone := false;
end;
{*************************************************}
procedure TJvSimpleXmlElemHeader.LoadFromStream(const Stream: TStream; Parent: TJvSimpleXml);
//<?xml version="1.0" encoding="iso-xyzxx" standalone="yes"?>
const
  CS_START_HEADER   =   '<?xml';
  CS_STOP_HEADER    =   '     ?>';
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := false;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        1..4: //<?xml
          if lBuf[i] = CS_START_HEADER[lPos] then
            inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_HEADER[lPos] +
              ' Found ' + lBuf[i]);

        5: //l
          if lBuf[i] = CS_START_HEADER[lPos] then
          begin
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            inc(lPos);

            FVersion := Properties.Value('version');
            FEncoding := Properties.Value('encoding');
            FStandalone := Properties.Value('standalone') = 'yes';

            Properties.Clear;

            Break; //Re read buffer
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_HEADER[lPos] +
              ' Found ' + lBuf[i]);

        6: //?
          if lBuf[i] = CS_STOP_HEADER[lPos] then
            inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_STOP_HEADER[lPos] +
              ' Found ' + lBuf[i]);

        7: //>
          if lBuf[i] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := true;
            Break; //End if
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_STOP_HEADER[lPos] +
              ' Found ' + lBuf[i]);
      end;
    end;
  until count=0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Name := '';

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemHeader.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
 st: string;
begin
  st := Level + '<?xml version="' + FVersion + '"';
  if Standalone then
    st := st+' standalone="yes"';
  if Encoding<>'' then
    st := st+' encoding="' + Encoding + '"';
  st := st + '?>' + sLineBreak;
  Stream.Write(st[1],Length(st));
  if Parent<>nil then
    Parent.DoSaveProgress;
end;
{*************************************************}

{ TJvSimpleXmlElemDocType }

{*************************************************}
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
  CS_START_DOCTYPE   =   '<!DOCTYPE';
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 lOk: Boolean;
 lChar: Char;
 st: string;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := false;
  lChar := '>';
  st := '';

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        1..9: //<!DOCTYPE
          if lBuf[i] = CS_START_DOCTYPE[lPos] then
            inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Header: Expected ' + CS_START_DOCTYPE[lPos] +
              ' Found ' + lBuf[i]);

        10: //]> or >
          if (lChar=lBuf[i]) then
          begin
            if (lChar='>') then
            begin
              lOk := true;
              Count := 0;
              Break; //This is the end
            end
            else
            begin
              st := st + lBuf[i];
              lChar := '>';
            end;
          end
          else
          begin
            st := st + lBuf[i];
            if lBuf[i]='[' then
              lChar := ']';
          end;
      end;
    end;
  until count=0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Comment: Unexpected end of data');

  Name := '';
  Value := Trim(st);

  if Parent<>nil then
    Parent.DoValueParsed('',st);

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemDocType.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
 st: string;
begin
  st := '<!DOCTYPE ' + Value + '>' + sLineBreak;
  Stream.Write(st[1],Length(st));
  if Parent<>nil then
    Parent.DoSaveProgress;
end;
{*************************************************}

{ TJvSimpleXmlElemSheet }

{*************************************************}
procedure TJvSimpleXmlElemSheet.LoadFromStream(const Stream: TStream;
  Parent: TJvSimpleXml);
//<?xml-stylesheet alternate="yes" type="text/xsl" href="sheet.xsl"?>
const
  CS_START_PI   =   '<?xml-stylesheet';
  CS_STOP_PI    =   '                ?>';
var
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := false;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        1..15: //<?xml-stylesheet
          if lBuf[i] = CS_START_PI[lPos] then
            inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_START_PI[lPos] +
              ' Found ' + lBuf[i]);

        16: //l
          if lBuf[i] = CS_START_PI[lPos] then
          begin
            Stream.Seek(lStreamPos, soFromBeginning);
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            inc(lPos);
            Break; //Re read buffer
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_START_PI[lPos] +
              ' Found ' + lBuf[i]);

        17: //?
          if lBuf[i] = CS_STOP_PI[lPos] then
            inc(lPos)
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_STOP_PI[lPos] +
              ' Found ' + lBuf[i]);

        18: //>
          if lBuf[i] = CS_STOP_PI[lPos] then
          begin
            Count := 0; //End repeat
            lOk := true;
            Break; //End if
          end
          else
            raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Expected ' + CS_STOP_PI[lPos] +
              ' Found ' + lBuf[i]);
      end;
    end;
  until count=0;

  if not lOk then
    raise TJvSimpleXmlInvalid.Create('Invalid Stylesheet: Unexpected end of data');

  Name := '';

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemSheet.SaveToStream(const Stream: TStream;
  const Level: string; Parent: TJvSimpleXml);
var
 i : integer;
 st: string;
begin
  st := Level + '<?xml-stylesheet';
  for i:=0 to Properties.GetCount-1 do
    st := st + Properties.Item[i].SaveToString;
  st := st + '?>' + sLineBreak;
  Stream.Write(st[1],Length(st));
  if Parent<>nil then
    Parent.DoSaveProgress;
end;
{*************************************************}

{ TJvSimpleXmlElemsProlog }

{*************************************************}
procedure TJvSimpleXmlElemsProlog.Clear;
begin
  while FElems.Count>0 do
  begin
    TJvSimpleXmlElem(FElems[0]).Free;
    FElems.Delete(0);
  end;
end;
{*************************************************}
constructor TJvSimpleXmlElemsProlog.Create;
begin
  FElems := THashedStringList.Create;
end;
{*************************************************}
destructor TJvSimpleXmlElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited;
end;
{*************************************************}
function TJvSimpleXmlElemsProlog.GetCount: Integer;
begin
  result := FElems.Count;
end;
{*************************************************}
function TJvSimpleXmlElemsProlog.GetItem(
  const Index: Integer): TJvSimpleXmlElem;
begin
  result := TJvSimpleXmlElem(FElems.Objects[Index]);
end;
{*************************************************}
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
 i, lStreamPos, count, lPos: Integer;
 lBuf: array[0..1024] of char;
 st: string;
 lEnd: Boolean;
 lElem: TJvSimpleXmlElem;
begin
  lStreamPos := Stream.Position;
  result := '';
  st := '';
  lPos := 0;

  repeat
    count := Stream.Read(lBuf,SizeOf(lBuf));
    if Parent<>nil then
      Parent.DoLoadProgress(Stream.Position,Stream.Size);
    for i:=0 to count-1 do
    begin
      //Increment stream pos for after comment
      inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces
          begin
            case lBuf[i] of
              ' ',#9,#13,#10:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  st := lBuf[i];
                end;
              else
                raise TJvSimpleXmlInvalid.Create('Invalid Document: Unexpected text in file prolog.');
            end;
          end;

        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            lEnd := false;

            st := st + lBuf[i];
            if st='<![CDATA[' then
              lEnd := true
            else if st='<!--' then
              lElem := TJvSimpleXmlElemComment.Create(nil)
            else if st='<?xml-stylesheet' then
              lElem := TJvSimpleXmlElemSheet.Create(nil)
            else if st='<?' then
              lElem := TJvSimpleXmlElemHeader.Create
            else if st='<!DOCTYPE' then
              lElem := TJvSimpleXmlElemDoctype.Create(nil)
            else if (Length(st)>1) and not (st[2] in ['!','?']) then
              lEnd := true;

            if lEnd then
            begin
              lStreamPos := lStreamPos - Length(st);
              Count := 0;
              Break;
            end
            else
            if lElem<>nil then
            begin
              Stream.Seek(lStreamPos - (Length(st)),soFromBeginning);
              lElem.LoadFromStream(Stream);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name,lElem);
              st := '';
              lPos := 0;
              Break;
            end;
          end;
      end;
    end;
  until count = 0;

  Stream.Seek(lStreamPos,soFromBeginning);
end;
{*************************************************}
procedure TJvSimpleXmlElemsProlog.SaveToStream(const Stream: TStream; Parent: TJvSimpleXml);
var
 i: Integer;
begin
  if Count=0 then
    FElems.AddObject('',TJvSimpleXmlElemHeader.Create);
  for i:=0 to Count-1 do
    Item[i].SaveToStream(Stream, '', Parent);
end;
{*************************************************}

{ TJvSimpleHashTable }

{*************************************************}
procedure TJvSimpleHashTable.AddObject(const AName: string;
  AObject: TObject);
begin
  //XXX
  new(FList^.FirstElem);
  //FList^.FirstElem^.Value := AName;
  //FList^.FirstElem^.Obj := nil;
end;
{*************************************************}
procedure TJvSimpleHashTable.Clear;
begin
  //XXX
end;
{*************************************************}
constructor TJvSimpleHashTable.Create;
begin
  //XXX
  new(FList);
  FList^.Count := 0;
  FList^.Kind := hkDirect;
  FList^.FirstElem := nil;
end;
{*************************************************}
destructor TJvSimpleHashTable.Destroy;
begin
  Clear;
  Dispose(FList);
  inherited;
end;
{*************************************************}

{ TXmlVariant }
{$IFDEF COMPILER6_UP}

{*************************************************}
function VarXml: TVarType;
begin
  Result := XmlVariant.VarType;
end;

{*************************************************}
procedure XmlCreateInto(var ADest: Variant; const AXml: TJvSimpleXmlElem);
begin
  TXmlVarData(ADest).VType := VarXml;
  TXmlVarData(ADest).Xml := AXml;
end;

{*************************************************}
function XmlCreate(const AXml: TJvSimpleXmlElem): Variant;
begin
  XmlCreateInto(result, AXml);
end;

{*************************************************}
function XmlCreate: Variant;
begin
  XmlCreateInto(result,TJvSimpleXmlElemClassic.Create(nil));
end;

{*************************************************}
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

{*************************************************}
procedure TXmlVariant.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  TXmlVarData(V).Xml := nil;
end;

{*************************************************}
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

{*************************************************}
function TXmlVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  lXml: TJvSimpleXmlElem;
  i,j,k: Integer;
begin
  result := false;
  if (Length(Arguments) = 1) and (Arguments[0].VType in [vtInteger,vtExtended]) then
    with TXmlVarData(V) do
    begin
      k := Arguments[0].vInteger;
      j := 0;

      if k>0 then
       for i:=0 to Xml.Items.Count-1 do
         if UpperCase(Xml.Items[i].Name) = Name then
         begin
           inc(j);
           if j=k then
             Break;
         end;

      if (j=k) and (j<Xml.Items.Count) then
      begin
        lXml := Xml.Items[j];
        if lXml<>nil then
        begin
          Dest.VType := VarXml;
          TXmlVarData(Dest).Xml := lXml;
          result := true;
        end
      end;
    end;
end;

{*************************************************}
function TXmlVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  lXml: TJvSimpleXmlElem;
  lProp: TJvSimpleXmlProp;
begin
  result := false;
  with TXmlVarData(V) do
  begin
    lXml := Xml.Items.ItemNamed[Name];
    if lXml<>nil then
    begin
      Dest.VType := VarXml;
      TXmlVarData(Dest).Xml := lXml;
      result := true;
    end
    else
    begin
      lProp := Xml.Properties.ItemNamed[Name];
      if lProp<>nil then
      begin
        VarDataFromOleStr(Dest, lProp.Value);
        result := true;
      end;
    end;
  end;
end;

{*************************************************}
function TXmlVariant.IsClear(const V: TVarData): Boolean;
begin
  Result := (TXmlVarData(V).Xml = nil) or
            (TXmlVarData(V).Xml.Items.Count = 0);
end;

{*************************************************}
function TXmlVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  lXml: TJvSimpleXmlElem;
  lProp: TJvSimpleXmlProp;

  function GetStrValue: string;
  begin
    try
      result := Value.VOleStr;
    except
      result := '';
    end;
  end;

begin
  result := false;
  with TXmlVarData(V) do
  begin
    lXml := Xml.Items.ItemNamed[Name];
    if lXml=nil then
    begin
      lProp := Xml.Properties.ItemNamed[Name];
      if lProp<>nil then
      begin
        lProp.Value := GetStrValue;
        result := true;
      end;
    end
    else
    begin
      lXml.Value := GetStrValue;
      result := true;
    end;
  end;
end;

initialization
  XmlVariant := TXmlVariant.Create;
finalization
  FreeAndNil(XmlVariant);
{$ENDIF}
end.

