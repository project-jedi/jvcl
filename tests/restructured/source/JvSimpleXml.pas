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

Contributor(s): _________________________________.

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues: This component has been simplified to be one of the fastest parser
 ever written, so, there is some compatibility issues with special encoding stuff's
 and it doesn't support comments.
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvSimpleXml;

interface

uses
  SysUtils, Classes, IniFiles, JvComponent{$IFDEF COMPILER6_UP}, Dialogs, Variants{$ENDIF};

type
{$IFNDEF COMPILER6_UP}
  THashedStringlist = class(TStringlist);
{$ENDIF}
  TJvSimpleXmlElem = class;
  TJvSimpleXmlElems = class;
  TJvSimpleXmlProps = class;
  TJvOnSimpleXmlParsed = procedure(Sender: TObject; Name: string) of object;
  TJvOnValueParsed = procedure(Sender: TObject; Name, Value: string) of object;
  TJvOnSimpleProgress = procedure(Sender: TObject; const Position, Total: Integer) of object;

  TJvSimpleXmlProp = class(TObject)
  private
    FName: string;
    FValue: string;
    FParent: TJvSimpleXmlProps;
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
  protected
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    property Parent: TJvSimpleXmlProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
  end;

  TJvSimpleXmlProps = class(TObject)
  private
    FProperties: THashedStringList;

    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXmlProp;
  protected
    function GetItem(const Index: Integer): TJvSimpleXmlProp;
    procedure Analyse(const Value: string);
    procedure DoItemRename(var Value: TJvSimpleXmlProp; const Name: string);
  public
    destructor Destroy; override;
    procedure Add(const Name, Value: string); overload;
    procedure Add(const Name: string; const Value: Int64); overload;
    procedure Add(const Name: string; const Value: Boolean); overload;
    procedure Clear;virtual;
    procedure Delete(const Index: Integer);overload;
    procedure Delete(const Name: string);overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = true): Boolean;

    property Item[const Index: Integer]: TJvSimpleXmlProp read GetItem; default;
    property ItemNamed[const Name: string]: TJvSimpleXmlProp read GetItemNamed;
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
    procedure AddChild(var Value: TJvSimpleXmlElem);
    procedure AddChildFirst(var Value: TJvSimpleXmlElem);
    procedure DoItemRename(var Value: TJvSimpleXmlElem; const Name: string);
    procedure CreateElems;
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    function Add(const Name: string): TJvSimpleXmlElem; overload;
    function Add(const Name, Value: string): TJvSimpleXmlElem; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXmlElem; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXmlElem; overload;
    function Add(const Name: string; const Value: TStream): TJvSimpleXmlElem; overload;
    function Add(Value: TJvSimpleXmlElem): TJvSimpleXmlElem; overload;
    procedure Clear;virtual;
    procedure Delete(const Index: Integer);overload;
    procedure Delete(const Name: string);overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = true): Boolean;
    procedure BinaryValue(const Name: string; const Stream: TStream);

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
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TJvSimpleXmlProps;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
    function GetItems: TJvSimpleXmlElems;
  protected
    procedure Analyse(const Value: string);
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    procedure Assign(Value: TJvSimpleXmlElem);
    procedure Clear;virtual;

    function SaveToString: string;
    procedure GetBinaryValue(const Stream: TStream);

    property ChildsCount: Integer read GetChildsCount;
    property Name: string read FName write SetName;
    property Parent: TJvSimpleXmlElem read FParent write FParent;
    property Items: TJvSimpleXmlElems read GetItems;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property Properties: TJvSimpleXmlProps read GetProps;
    property Value: string read FValue write FValue;
  end;

  TJvSimpleXml = class(TJvComponent)
  private
    FFileName: TFileName;
    FRoot: TJvSimpleXmlElem;
    FOnTagParsed: TJvOnSimpleXmlParsed;
    FOnValue: TJvOnValueParsed;
    FEncoding: string;
    FOnLoadProg: TJvOnSimpleProgress;
    FOnSaveProg: TJvOnSimpleProgress;
  protected
    procedure SetFileName(Value: TFileName);
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

    property Root: TJvSimpleXmlElem read FRoot write FRoot;
  published
    property Encoding: string read FEncoding write FEncoding;
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
begin
  //http://www.cs.tut.fi/~jkorpela/latin1/3.html#60
  result := '';
  for i := 1 to Length(Value) do
    case Value[i] of
      '<', '>', '&', '"', '''':
        result := result + '&#' + IntToStr(Ord(Value[i])) + ';';
    else
      result := Result + Value[i];
    end;
end;
{*************************************************}

function SimpleXmlDecode(Value: string): string;
var
  i, j: Integer;
  st: string;
begin
  result := '';
  st := '';
  j := -1;
  for i := 1 to Length(Value) do
    case Value[i] of
      '&':
        begin
          if j <> -1 then
            result := result + '&' + st;
          j := 0;
          st := '';
        end;
      '#':
        if j = 0 then
          j := 1
        else if (j <> -1) then
        begin
          result := result + '&' + st + Value[i];
          st := '';
        end
        else
        begin
          result := result + st + Value[i];
          st := '';
        end;
      '0'..'9':
        if j >= 1 then
        begin
          st := st + Value[i];
          j := 2;
        end
        else
          result := result + Value[i];
      'a'..'z', 'A'..'Z':
        if j >= 0 then
        begin
          st := st + Value[i];
          inc(j);
        end
        else
          result := result + Value[i];
      ';':
        if j <> 0 then
        begin
          j := StrToIntDef(st, -1);
          case j of
            -1:
              begin
                st := LowerCase(st);
                if st = 'lt' then
                  result := result + '<'
                else if st = 'gt' then
                  result := result + '>'
                else if j>=0 then
                  result := result + '&' + st + ';'
                else
                  result := result + ';';
              end;
            0..100:
              result := result + Char(j);
            233:
              result := result + 'é';
            232:
              result := result + 'è';
          end;
          j := -1;
        end
        else
          result := result + Value[i]
      else
        begin
          if j > 0 then
            result := result + '&' + st
          else if j = 0 then
            result := result + '&';
          result := result + Value[i];
          j := -1;
        end;
    end;
  if j <> -1 then
    result := result + '&' + st;
end;
{*************************************************}

{*************************************************}

constructor TJvSimpleXml.Create(AOwner: TComponent);
begin
  inherited;
  FRoot := TJvSimpleXmlElem.Create(nil);
  FEncoding := 'iso-8859-1';
end;
{*************************************************}

destructor TJvSimpleXml.Destroy;
begin
  FreeAndNil(FRoot);
  inherited;
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
var
  Buffer: string;

  function ReadTag: string;
  var
    buf: array[0..1024] of char;
    i, count, k, l: Integer;
    ch: char;
  begin
    result := '';
    k := Stream.Position;
    l := 0;
    repeat
      count := Stream.Read(buf, SizeOf(buf));
      i := 0;
      while i < count do
      begin
        ch := buf[i];
        if ch = '<' then
        begin
          if l = 0 then
            inc(l);
          inc(i);
        end
        else if ch = '>' then
        begin
          if l = 1 then
          begin
            i := MAXINT;
            inc(l);
          end;
        end
        else
        begin
          result := result + ch;
          inc(i);
        end;
        inc(k);
      end;
    until (l = 2) or (count = 0);
    if count = 0 then
      raise EJVCLException.Create(RS_INVALID_SimpleXml)
    else
      Stream.Seek(k,soFromBeginning);
    result := Trim(Result);

    if Assigned(FOnLoadProg) then
      FOnLoadProg(self,Stream.Position,Stream.Size);
  end;

  function ReadValue: string;
  var
    buf: array[0..1024] of char;
    i, count, k: Integer;
    ok: Boolean;
  begin
    result := '';
    ok := false;
    k := Stream.Position;
    count := 1;
    while (not ok) and (count <> 0) do
    begin
      count := Stream.Read(buf, SizeOf(buf));
      i := 0;
      while i < count do
      begin
        if buf[i] = '<' then
        begin
          i := MAXINT;
          ok := true;
        end
        else
        begin
          result := result + buf[i];
          inc(k);
          inc(i);
        end;
      end;
    end;
    if count = 0 then
      raise EJVCLException.Create(RS_INVALID_SimpleXml)
    else
      Stream.Seek(k,soFromBeginning);
  end;

  procedure Analyse(var Element: TJvSimpleXmlElem);
  var
    data: string;
    TagEnded: boolean;
    ChildElem: TJvSimpleXmlElem;
  begin
    TagEnded := false;

    while (not TagEnded) do
    begin
      data := ReadTag;
      if (data = '/' + Element.Name) then
        TagEnded := true
      else if data[length(data)] = '/' then
      begin
        data := copy(data, 1, length(data) - 1);
        ChildElem := TJvSimpleXmlElem.Create(Element);
        ChildElem.Analyse(data);
        if Assigned(FOnTagParsed) then
          FOnTagParsed(self, ChildElem.Name);
      end
      else
      begin
        ChildElem := TJvSimpleXmlElem.Create(Element);
        ChildElem.Analyse(data);
        ChildElem.Value := SimpleXmlDecode(ReadValue);
        Analyse(ChildElem);
        if Assigned(FOnValue) then
          FOnValue(self, ChildElem.Name, ChildElem.Value);
      end;
    end;
  end;

begin
  FRoot.Clear;
  Buffer := '';

  //Get the first tag <...>
  repeat
    Buffer := ReadTag;
  until (pos('?', Buffer) <> 1) and (pos('!', Buffer) <> 1);
  FRoot.Analyse(SimpleXmlDecode(Buffer));

  //Get all the childs
  Analyse(FRoot);
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
  SaveToStream(Stream);
  Stream.Free;
end;
{*************************************************}

procedure TJvSimpleXml.SaveToStream(const Stream: TStream);
var
 lCount,lCurrent: Integer;

  procedure WriteString(Value: string);
  begin
    Value := Value + #13#10;
    Stream.Write(Value[1], Length(Value));
  end;

  procedure WriteElement(const Value: TJvSimpleXmlElem; Prefix: string);
  var
    st: string;
    i: Integer;
  begin
    st := '<' + Value.Name;
    for i := 0 to Value.Properties.Count - 1 do
      st := st + ' ' + Value.Properties.Item[i].Name + '="' +
        SimpleXmlEncode(Value.Properties.Item[i].Value) + '"';

    if Value.Items.Count = 0 then
    begin
      if Value.Value = '' then
        WriteString(Prefix + st + '/>')
      else
        WriteString(Prefix + st + '>' + SimpleXmlEncode(Value.Value) + '</' + Value.Name + '>');
    end
    else
    begin
      WriteString(Prefix + st + '>');
      for i := 0 to Value.Items.Count - 1 do
        WriteElement(Value.Items[i], Prefix + ' ');
      WriteString(Prefix + '</' + Value.Name + '>');
    end;

    inc(lCurrent);
    if Assigned(FOnSaveProg) then
      FOnSaveProg(self,lCurrent,lCount);
  end;

begin
  lCount := Root.ChildsCount;
  lCurrent := 0;
  if Assigned(FOnSaveProg) then
    FOnSaveProg(self,0,lCount);
  WriteString('<?xml version="1.0" encoding="'+FEncoding+'" ?>');
  WriteElement(Root, '');
  if Assigned(FOnSaveProg) then
    FOnSaveProg(self,lCount,lCount);
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

procedure TJvSimpleXmlElem.Analyse(const Value: string);
var
  i, j: Integer;
begin
  i := 0;
  for j:=1 to Length(Value) do
    if (Value[j] in [' ', #10, #13]) then
    begin
      i := j;
      Break;
    end;

  if i = 0 then
    Name := Value
  else
  begin
    Name := Copy(Value, 1, i - 1);
    Properties.Analyse(Trim(Copy(Value, i + 1, MAXINT)));
  end;
  if Parent <> nil then
    Parent.Items.AddChild(self);
end;
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
  result := StrToBool(Value);
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
  st: string;
  i: Integer;
begin
  with TStringList.Create do
  begin
    st := '<' + Name;
    for i := 0 to Properties.Count - 1 do
      st := st + ' ' + Properties[i].Name + '="' + SimpleXmlEncode(Properties[i].Value) + '"';
    if Items.Count = 0 then
    begin
      st := st + '/>';
      Add(st);
    end
    else
    begin
      st := st + '>';
      Add(st);
      for i := 0 to Items.Count - 1 do
        Add(Items[i].SaveToString);
      Add('</' + Name + '>');
    end;
    Result := Text;
    Free;
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

function TJvSimpleXmlElems.Add(const Name: string): TJvSimpleXmlElem;
begin
  result := TJvSimpleXmlElem.Create(Parent);
  result.FName := Name; //Directly set parent to avoid notification 
  AddChild(result);
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name, Value: string): TJvSimpleXmlElem;
begin
  result := TJvSimpleXmlElem.Create(Parent);
  result.Name := Name;
  result.Value := Value;
  AddChild(result);
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name: string; const Value: Int64): TJvSimpleXmlElem;
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
  const Value: Boolean): TJvSimpleXmlElem;
begin
  result := Add(Name, BoolToStr(Value));
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name: string;
  const Value: TStream): TJvSimpleXmlElem;
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

procedure TJvSimpleXmlElems.AddChild(var Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.AddObject(Value.Name,Value);
end;
{*************************************************}

procedure TJvSimpleXmlElems.AddChildFirst(var Value: TJvSimpleXmlElem);
begin
  CreateElems;
  FElems.AddObject(Value.Name,Value)
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
  elem := GetItemNamed(Name);
  if elem = nil then
    result := Default
  else
    result := elem.BoolValue;
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

procedure TJvSimpleXmlProps.Add(const Name, Value: string);
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
end;
{*************************************************}

procedure TJvSimpleXmlProps.Add(const Name: string; const Value: Int64);
begin
  Add(Name, IntToStr(Value));
end;
{*************************************************}

procedure TJvSimpleXmlProps.Add(const Name: string; const Value: Boolean);
begin
  Add(Name, BoolToStr(Value));
end;
{*************************************************}

procedure TJvSimpleXmlProps.Analyse(const Value: string);
var
  st: string;
  i: Integer;
  last: char;

  procedure AnalyseStr(const Value: string);
  var
    i, j: Integer;
    st, st2: string;
  begin
    j := 0;
    for i:=1 to Length(Value) do
      if Value[i] = '=' then
      begin
        j := i;
        Break;
      end
      else
        st := st + Value[i];
    if j <> 0 then
    begin
      st2 := Copy(Value, j + 1, MAXINT);
      st2 := StringReplace(st2, '"', '', [rfReplaceAll]);
      st2 := StringReplace(st2, '''', '', [rfReplaceAll]);
      st2 := SimpleXmlDecode(st2)
    end
    else
      st2 := '';
    Add(st, st2);
  end;

begin
  st := '';
  last := ' ';
  for i := 1 to Length(Value) do
  begin
    case Value[i] of
      ' ':
        if (last = ' ') then
        begin
          if st <> '' then
          begin
            AnalyseStr(st);
            st := '';
          end;
        end
        else
          st := st + ' ';
      '"', '''':
        begin
          st := st + Value[i];
          if last = ' ' then
            last := Value[i]
          else
            if last = Value[i] then
            begin
              AnalyseStr(st);
              st := '';
              last := ' ';
            end;
        end;
    else
      st := st + Value[i];
    end;
  end;
end;
{*************************************************}

function TJvSimpleXmlProps.BoolValue(const Name: string;
  Default: Boolean): Boolean;
var
  prop: TJvSimpleXmlProp;
begin
  prop := GetItemNamed(Name);
  if prop = nil then
    result := Default
  else
    result := prop.BoolValue;
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
  result := StrToBool(Value);
end;
{*************************************************}

function TJvSimpleXmlProp.GetIntValue: Int64;
begin
  result := StrToInt64Def(Value, -1);
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
  XmlCreateInto(result,TJvSimpleXmlElem.Create(nil));
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

