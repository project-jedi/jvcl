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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvSimpleXml;

interface


uses
  SysUtils, Classes, IniFiles, JvComponent;

{$IfNDef THashedStringList}
type THashedStringList=TStringList;
{$EndIf}

type
  TJvSimpleXmlElem = class;
  TJvSimpleXmlElems = class;
  TJvSimpleXmlProps = class;
  TJvOnSimpleXmlParsed = procedure(Sender: TObject; Name: string) of object;
  TJvOnValueParsed = procedure(Sender: TObject; Name, Value: string) of object;

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
    procedure Analyse(Value: string);
    procedure DoItemRename(var Value: TJvSimpleXmlProp; const Name: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name, Value: string); overload;
    procedure Add(const Name: string; const Value: Int64); overload;
    procedure Add(const Name: string; const Value: Boolean); overload;
    procedure Clear;
    procedure Delete(const Index: Integer);
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

    function GetCount: Integer;
    function GetItemNamed(const Name: string): TJvSimpleXmlElem;
  protected
    function GetItem(const Index: Integer): TJvSimpleXmlElem;
    procedure AddChild(var Value: TJvSimpleXmlElem);
    procedure AddChildFirst(var Value: TJvSimpleXmlElem);
    procedure DoItemRename(var Value: TJvSimpleXmlElem; const Name: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Name: string): TJvSimpleXmlElem; overload;
    function Add(const Name, Value: string): TJvSimpleXmlElem; overload;
    function Add(const Name: string; const Value: Int64): TJvSimpleXmlElem; overload;
    function Add(const Name: string; const Value: Boolean): TJvSimpleXmlElem; overload;
    function Add(const Name: string; const Value: TStream): TJvSimpleXmlElem; overload;
    function Add(Value: TJvSimpleXmlElem): TJvSimpleXmlElem; overload;
    procedure Clear;
    procedure Delete(const Index: Integer);overload;
    procedure Delete(const Name: string);overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = true): Boolean;
    procedure BinaryValue(const Name: string; const Stream: TStream);

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
    procedure SetIntValue(const Value: Int64);
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
  protected
    procedure Analyse(Value: string);
  public
    constructor Create(const AOwner: TJvSimpleXmlElem);
    destructor Destroy; override;
    procedure Assign(Value: TJvSimpleXmlElem);
    procedure Clear;

    function SaveToString: string;
    procedure GetBinaryValue(const Stream: TStream);

    property Name: string read FName write SetName;
    property Parent: TJvSimpleXmlElem read FParent write FParent;
    property Items: TJvSimpleXmlElems read FItems;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property Properties: TJvSimpleXmlProps read FProps;
    property Value: string read FValue write FValue;
  end;

  TJvSimpleXml = class(TJvComponent)
  private
    FFileName: TFileName;
    FRoot: TJvSimpleXmlElem;
    FOnTagParsed: TJvOnSimpleXmlParsed;
    FOnValue: TJvOnValueParsed;
    FEncoding: string;
  protected
    procedure SetFileName(Value: TFileName);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    function SaveToString: string;

    property Root: TJvSimpleXmlElem read FRoot write FRoot;
  published
    property Encoding: string read FEncoding write FEncoding;
    property FileName: TFileName read FFileName write SetFileName;
    property OnTagParsed: TJvOnSimpleXmlParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TJvOnValueParsed read FOnValue write FOnValue;
  end;

resourcestring
  RS_INVALID_SimpleXml = 'Invalid XML file';
{$IFNDEF COMPILER6_UP}
  SInvalidBoolean = '''%s'' is not a valid boolean value';
{$ENDIF COMPILER6_UP}

implementation

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
  FEncoding := 'windows-1252';
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
  Stream.LoadFromFile(FileName);
  LoadFromStream(Stream);
  Stream.Free;
end;
{*************************************************}

procedure TJvSimpleXml.LoadFromStream(const Stream: TStream);
var
  Buffer: string;

  function ReadTag: string;
  var
    buf: array[0..100] of char;
    i, count, k, l: Integer;
  begin
    result := '';
    k := Stream.Position;
    l := 0;
    count := 1;
    while (l <> 2) and (count <> 0) do
    begin
      count := Stream.Read(buf, SizeOf(buf));
      i := 0;
      while i < count do
      begin
        if buf[i] = '<' then
        begin
          if l = 0 then
            inc(l);
          inc(i);
        end
        else if buf[i] = '>' then
        begin
          if l = 1 then
          begin
            i := MAXINT;
            inc(l);
          end;
        end
        else
        begin
          result := result + buf[i];
          inc(i);
        end;
        inc(k);
      end;
    end;
    if count = 0 then
      raise Exception.Create(RS_INVALID_SimpleXml)
    else
      Stream.Position := k;
    result := Trim(Result);
  end;

  function ReadValue: string;
  var
    buf: array[0..100] of char;
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
      raise Exception.Create(RS_INVALID_SimpleXml)
    else
      Stream.Position := k;
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
  end;

begin
  WriteString('<?xml version="1.0" encoding="'+FEncoding+'" ?>');
  WriteElement(Root, '');
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

procedure TJvSimpleXmlElem.Analyse(Value: string);
var
  i, j: Integer;
begin
  i := 0;
  j := 1;
  while (j <= Length(Value)) do
  begin
    if (Value[j] in [' ', #10, #13]) then
    begin
      i := j;
      j := MAXINT;
    end
    else
      inc(j);
  end;

  if i = 0 then
    Name := Value
  else
  begin
    Name := Copy(Value, 1, i - 1);
    Value := Trim(Copy(Value, i + 1, MAXINT));
    Properties.Analyse(Value);
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
  Items.Clear;
  Properties.Clear;
end;
{*************************************************}

constructor TJvSimpleXmlElem.Create(const AOwner: TJvSimpleXmlElem);
begin
  FName := '';
  FParent := TJvSimpleXmlElem(AOwner);
  FItems := TJvSimpleXmlElems.Create();
  FProps := TJvSimpleXmlProps.Create();
end;
{*************************************************}

destructor TJvSimpleXmlElem.Destroy;
begin
  FItems.Free;
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

function TJvSimpleXmlElem.GetIntValue: Int64;
begin
  result := StrToInt64Def(Value, -1);
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
  result := TJvSimpleXmlElem.Create(nil);
  result.Name := Name;
  AddChild(result);
end;
{*************************************************}

function TJvSimpleXmlElems.Add(const Name, Value: string): TJvSimpleXmlElem;
begin
  result := TJvSimpleXmlElem.Create(nil);
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
  FElems.AddObject(Value.Name,Value);
end;
{*************************************************}

procedure TJvSimpleXmlElems.AddChildFirst(var Value: TJvSimpleXmlElem);
begin
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
begin
  FElems.Clear;
end;
{*************************************************}

constructor TJvSimpleXmlElems.Create;
begin
  FElems := THashedStringList.Create;
end;
{*************************************************}

procedure TJvSimpleXmlElems.Delete(const Index: Integer);
begin
  if (Index>=0) and (Index<FElems.Count) then
    FElems.Delete(Index);
end;
{*************************************************}

procedure TJvSimpleXmlElems.Delete(const Name: string);
begin
  Delete(FElems.IndexOf(Name));
end;
{*************************************************}

destructor TJvSimpleXmlElems.Destroy;
begin
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
  result := FElems.Count;
end;
{*************************************************}

function TJvSimpleXmlElems.GetItem(const Index: Integer): TJvSimpleXmlElem;
begin
  if Index > FElems.Count then
    result := nil
  else
    result := TJvSimpleXmlElem(FElems.Objects[Index]);
end;
{*************************************************}

function TJvSimpleXmlElems.GetItemNamed(const Name: string): TJvSimpleXmlElem;
var
  i: Integer;
begin
  i := FElems.IndexOf(Name);
  if i<>-1 then
    result := TJvSimpleXmlElem(FElems.Objects[i])
  else
    result := nil;
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
  Elem := TJvSimpleXmlProp.Create();
  FProperties.AddObject(Name,Elem);
  Elem.Name := Name;
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

procedure TJvSimpleXmlProps.Analyse(Value: string);
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
    i := 1;
    while i < Length(Value) do
      if Value[i] = '=' then
      begin
        j := i;
        i := MAXINT;
      end
      else
      begin
        st := st + Value[i];
        inc(i);
      end;
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
begin
  FProperties.Clear;
end;
{*************************************************}

constructor TJvSimpleXmlProps.Create;
begin
  FProperties := THashedStringList.Create;
end;
{*************************************************}

procedure TJvSimpleXmlProps.Delete(const Index: Integer);
begin
  FProperties.Delete(Index);
end;
{*************************************************}

destructor TJvSimpleXmlProps.Destroy;
begin
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
  result := FProperties.Count;
end;
{*************************************************}

function TJvSimpleXmlProps.GetItem(const Index: Integer): TJvSimpleXmlProp;
begin
  result := TJvSimpleXmlProp(FProperties.Objects[Index]);
end;
{*************************************************}

function TJvSimpleXmlProps.GetItemNamed(const Name: string): TJvSimpleXmlProp;
var
  i: Integer;
begin
  i := FProperties.IndexOf(Name);
  if i<>-1 then
    result := TJvSimpleXmlProp(FProperties.Objects[i])
  else
    result := nil;
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

end.

