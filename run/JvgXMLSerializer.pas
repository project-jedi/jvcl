{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgXMLSerializer.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:
  The component converts given component to XML and back according to
  published interface of its class.

  XML is made of tags pairs with values put inside. Tags can have no attributes

  Topmost tag matches class of the object. Inner tags match properties' names.
  For TCollectionItem containing tag matches the name of the class

  Tags' nesting is unlimited and repeats(reproduces) the whole published
  interface of class of the given object

  The following types are supported: integer numbers, floats, enumerations,
  sets, strings and chars, variants, classes, stringlists and collections.

  Interface:
    procedure Serialize(Component: TObject; Stream: TStream);
    - Serialization TPersistent -> XML
    procedure DeSerialize(Component: TObject; Stream: TStream);
    - Loading XML -> TPersistent

    property GenerateFormattedXML       - Generate Formatted XML
    property ExcludeEmptyValues         - Skip properties with empty values
    property ExcludeDefaultValues       - Skip properties with default values
    property StrongConformity           - Requires XML to has all the corresponding
                                          tags for all class types
    property IgnoreUnknownTags          - ignore unknown tags when loading XML
    property OnGetXMLHeader             - Allows to specifies one's own XML header //AFAIR - topmost XML tag

    WrapCollections - Wrap collections in individual(dedicated) tags

  Limitations:
    Each object can have only one collection per collection item class
    TStrings derivatives must have no published properties
    Procedure types are not supported

    To generate DTD it needs object to has all class-properties, with names same
    to properties of agregated objects, of single(the same, "one") class

  Preconditions:
    Object for de-serializatino into, is to be created prior to procedure's call.

    Is StringConformity then loading XML must contain tags for all the class-types.
    Presence of other tags is not checked.

  Extra:
    When loading TCollection from XML, it is not voided (?) so you can load
    TCollection as a merge of different XML sources.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgXMLSerializer;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, TypInfo,
  JvComponentBase;

type
  TOnGetXMLHeader = procedure(Sender: TObject; var Value: string) of object;
  TBeforeParsingEvent = procedure(Sender: TObject; Buffer: PChar) of object;

  EJvgXMLSerializerException = class(Exception);
  XMLSerializerException = class(Exception);
  EJvgXMLOpenTagNotFoundException = class(XMLSerializerException);
  EJvgXMLCloseTagNotFoundException = class(XMLSerializerException);
  EJvgXMLUnknownPropertyException = class(XMLSerializerException);

  TJvgXMLSerializerException = class of XMLSerializerException;

  TJvgXMLSerializer = class(TJvComponent)
  private
    Buffer: PChar;
    BufferEnd: PChar;
    BufferLength: DWORD;
    TokenPtr {, MaxTokenPtr}: PChar;
    OutStream: TStream;
    FOnGetXMLHeader: TOnGetXMLHeader;
    FGenerateFormattedXML: Boolean;
    FExcludeEmptyValues: Boolean;
    FExcludeDefaultValues: Boolean;
    FReplaceReservedSymbols: Boolean;
    FStrongConformity: Boolean;
    FBeforeParsing: TBeforeParsingEvent;
    FWrapCollections: Boolean;
    FIgnoreUnknownTags: Boolean;
    procedure Check(Expr: Boolean; const Msg: string; E: TJvgXMLSerializerException);
    procedure WriteOutStream(const Value: string);
  protected
    procedure SerializeInternal(Component: TObject; Level: Integer = 1);
    procedure DeSerializeInternal(Component: TObject;
      ComponentTagName: string; ParentBlockEnd: PChar = nil);
    procedure GenerateDTDInternal(Component: TObject; DTDList: TStrings;
      Stream: TStream; const ComponentTagName: string);
    procedure SetPropertyValue(Component: TObject; PropInfo: PPropInfo; Value,
      ValueEnd: PChar; ParentBlockEnd: PChar);
  public
    DefaultXMLHeader: string;
    tickCounter: DWORD;
    tickCount: DWORD;
    constructor Create(AOwner: TComponent); override;
    //{ ������������ ������� � XML }
    { Serialization of object to XML [translated] }
    procedure Serialize(Component: TObject; Stream: TStream);
    //{ �������� XML � ������ }
    { Loading XML into object [translated] }
    procedure DeSerialize(Component: TObject; Stream: TStream);
    //{ ��������� DTD }
    { Genereating DTD [translated] }
    procedure GenerateDTD(Component: TObject; Stream: TStream);
  published
    property GenerateFormattedXML: Boolean read FGenerateFormattedXML write FGenerateFormattedXML default True;
    property ExcludeEmptyValues: Boolean read FExcludeEmptyValues write FExcludeEmptyValues;
    property ExcludeDefaultValues: Boolean read FExcludeDefaultValues write FExcludeDefaultValues;
    property ReplaceReservedSymbols: Boolean read FReplaceReservedSymbols write FReplaceReservedSymbols;
    property StrongConformity: Boolean read FStrongConformity write FStrongConformity default True;
    property IgnoreUnknownTags: Boolean read FIgnoreUnknownTags write FIgnoreUnknownTags;
    property WrapCollections: Boolean read FWrapCollections write FWrapCollections default True;
    property OnGetXMLHeader: TOnGetXMLHeader read FOnGetXMLHeader write FOnGetXMLHeader;
    property BeforeParsing: TBeforeParsingEvent read FBeforeParsing write FBeforeParsing;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvResources, JvgUtils, JclSysUtils;

const
  ORDINAL_TYPES = [tkInteger, tkChar, tkEnumeration, tkSet];

var
  TAB: string;
  CR: string;

constructor TJvgXMLSerializer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //...defaults
  FGenerateFormattedXML := True;
  FStrongConformity := True;
  FWrapCollections := True;
end;

//{ ����� ������ � ��������� �����. ���-�� ��� ������������ }
{ writes string to output stream. Used for serialization. [translated] }

procedure TJvgXMLSerializer.WriteOutStream(const Value: string);
var
  AnsiValue: AnsiString;
begin
  if Value <> '' then
  begin
    AnsiValue := AnsiString(Value);
    OutStream.Write(AnsiValue[1], Length(AnsiValue));
  end;
end;

//  ������������ ��������� � XML-��� � ������������
//  � published ����������� ������ �������.
// ����:
//    Component - ��������� ��� �����������
//  �����:
//    ����� XML � ����� Stream

{
  Converts component to XML, according to published interface of its class
  Input:
    Component - Component to be converted
  Output:
    XML text into Stream
}

procedure TJvgXMLSerializer.Serialize(Component: TObject; Stream: TStream);
var
  Result: string;
begin
  TAB := IIF(GenerateFormattedXML, #9, '');
  CR := IIF(GenerateFormattedXML, #13#10, '');

  Result := '';
  //{ ��������� XML ��������� }
  { Retrieving XML header [translated] }
  if Assigned(OnGetXMLHeader) then
    OnGetXMLHeader(Self, Result);
  if Result = '' then
    Result := DefaultXMLHeader;

  OutStream := Stream;

  WriteOutStream(Result);

  WriteOutStream(CR + '<' + Component.ClassName + '>');
  SerializeInternal(Component);
  WriteOutStream(CR + '</' + Component.ClassName + '>');
end;

//  ���������� ��������� ����������� ������� � XML
//  ���������� ��:
//    Serialize()
//  ����:
//    Component - ��������� ��� �����������
//    Level - ������� ����������� ���� ��� �������������� ����������
//  �����:
//    ������ XML � �������� ����� ����� ����� WriteOutStream()

{
  Internal procedure Object->XML
  Is called from:
    Serialize()
  Input:
    Component - Component to be converted
    Level     - Level of nesting (for formatted output)
  Output:
    XML string into output Stream via .WriteOutStream() method
}

procedure TJvgXMLSerializer.SerializeInternal(Component: TObject; Level: Integer = 1);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  I, J: Integer;
  AName, PropName, sPropValue: string;
  PropList: PPropList;
  NumProps: Word;
  PropObject: TObject;

  //{ ��������� ����������� ��� � �������� ������ }
  { Adds opening tag with given name  [translated] }

  procedure addOpenTag(const Value: string);
  begin
    WriteOutStream(CR + DupStr(TAB, Level) + '<' + Value + '>');
    Inc(Level);
  end;

  //{ ��������� ����������� ��� � �������� ������ }
  { Adds closing tag with given name  [translated] }

  procedure addCloseTag(const Value: string; AddBreak: Boolean = False);
  begin
    Dec(Level);
    if AddBreak then
      WriteOutStream(CR + DupStr(TAB, Level));
    WriteOutStream('</' + Value + '>');
  end;

  //{ ��������� �������� � �������������� ������ }
  { Adds value [in]to result string  [translated] }

  procedure addValue(const Value: string);
  begin
    WriteOutStream(Value);
  end;

begin
  //  Result := '';

  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(TypeInf^.Name);
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps * SizeOf(Pointer));
  try
    //{ �������� ������ ������� }
    { Getting list of properties  [translated] }
    GetPropInfos(TypeInf, PropList);

    for I := 0 to NumProps - 1 do
    begin
      PropName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropList^[I]^.Name);

      PropTypeInf := PropList^[I]^.PropType^;
      PropInfo := PropList^[I];

      //{ ����� �� ��������, ����� ��� ��������� ? }
      { Does the property wish to be saved?  [translated] }
      if not IsStoredProp(Component, PropInfo) then
        Continue;

      case PropTypeInf^.Kind of
        tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
        {$IFDEF UNICODE} tkUString, {$ENDIF}
        tkWChar, tkLString, tkWString, tkVariant:
          begin
            //{ ��������� �������� �������� }
            { Getting property's value  [translated] }
            sPropValue := GetPropValue(Component, PropName, True);

            //{ ��������� �� ������ �������� � �������� �� ��������� }
            { Checking if value is empty or is default  [translated] }
            if ExcludeEmptyValues and (sPropValue = '') then
              Continue;
            if ExcludeDefaultValues and (PropTypeInf^.Kind in ORDINAL_TYPES) and
              (sPropValue = IntToStr(PropInfo.Default)) then
              Continue;

            //{ ������ ������������ }
            { special characters placeholders  [translated] }
            if FReplaceReservedSymbols then
            begin
              sPropValue := StringReplace(sPropValue, '<', '&lt;',
                [rfReplaceAll]);
              sPropValue := StringReplace(sPropValue, '>', '&gt;',
                [rfReplaceAll]);
              // sPropValue := StringReplace(sPropValue, '&', '&', [rfReplaceAll]);
            end;

            //{ ������� � XML }
            { converting to XML  [translated] }
            addOpenTag(PropName);
            //{ ��������� �������� �������� � ��������� }
            { adds property's value to result  [translated] }
            addValue(sPropValue);
            addCloseTag(PropName);
          end;
        tkClass:
          //{ ��� ��������� ����� ����������� ��������� }
          { make recursive call for class-types  [translated] }
          begin
            PropObject := GetObjectProp(Component, PropInfo);
            if Assigned(PropObject) then
            begin
              //{ ��� �������� �������-������� - ����������� ����� }
              { make recursive call for children class-types   [translated] }

              //{ �������������� ������ � ��������� ������� }
              { Specific handlers for some certain classes  [translated] }
              if PropObject is TStrings then
              //{ ��������� ������ }
              { text lists  [translated] }
              begin
                addOpenTag(PropName);
                WriteOutStream(TStrings(PropObject).CommaText);
                addCloseTag(PropName, True);
              end
              else
              if PropObject is TCollection then
              //{ ��������� }
              { collections  [translated] }
              begin
                if WrapCollections then
                  addOpenTag(PropName);

                SerializeInternal(PropObject, Level);
                for J := 0 to (PropObject as TCollection).Count - 1 do
                begin
                  //{ ������������ ��� �� ����� ������ }
                  { Container-tag with name of the class  [translated] }
                  addOpenTag(TCollection(PropObject).Items[J].ClassName);
                  SerializeInternal(TCollection(PropObject).Items[J],
                    Level);
                  addCloseTag(TCollection(PropObject).Items[J].ClassName, True);
                end;

                if WrapCollections then
                  addCloseTag(PropName, True);
              end
              else
              if PropObject is TPersistent then
              begin
                addOpenTag(PropName);
                SerializeInternal(PropObject, Level);
                addCloseTag(PropName, True);
              end;

              //{ ����� ����� �������� ��������� ��������� �������: TTreeNodes, TListItems }
              { Here one can add handling of other classes like TreeNodes, TListItems  [translated] }
            end;
            //{ ����� ��������� ������� ��������� ��� ������� }
            { Closing object's tag after proceeded its properties  [translated] }
          end;
      end;
    end;
  finally
    FreeMem(PropList, NumProps * SizeOf(Pointer));
  end;
end;

//  ��������� � ��������� ������ �� ������ � XML-�����.
//  ����:
//    Component - ��������� ��� �����������
//    Stream - �������� �������� XML
//  �����������:
//    ������ Component ������ ���� ������ �� ������ ���������

{
  Loads component's properties ("data") from stream, containing XML stream
  Input:
    Component - Component to be convertes.
    Stream    - Stream containing XML to load
  Preconditions:
    Components object was created prior to procedure's call
}

procedure TJvgXMLSerializer.DeSerialize(Component: TObject; Stream: TStream);
var
  Buf: AnsiString;
  S: string;
begin
  SetLength(Buf, Stream.Size + 1);
  //{ �������� ������ �� ������ }
  { Retrievign data from stream  [translated] }
  if Buf <> '' then
    Stream.Read(Buf[1], Length(Buf));
  S := string(Buf);
  Buf := ''; // release unused memory
  Buffer := PChar(S);

  if Assigned(BeforeParsing) then
    BeforeParsing(Self, Buffer);

  //{ ������������� ������� ��������� ������ ������ }
  { Setting current pointer of reading data  [translated] }
  TokenPtr := Buffer;
  BufferLength := Length(S);
  BufferEnd := Buffer + BufferLength;
  //{ �������� ��������� }
  { Calling loader  [translated] }
  DeSerializeInternal(Component, Component.ClassName);
end;

//  ����������� ��������� �������� ������� �� ���������� ������ � XML
//  ���������� ��:
//    Serialize()
//  ����:
//    Component - ��������� ��� �����������
//    ComponentTagName - ��� XML ���� �������
//    ParentBlockEnd - ��������� �� ����� XML �������� ������������� ����

{
  Recursive procedure for loading of object from text buffer, containing XML
  Called from::
    Serialize()
  Input:
    Component        - Component to be converted,
    ComponentTagName - Name of XML tag for object (Arioch: may differ from
                       ClassName for CollectionItems, for XML header),
    ParentBlockEnd   - Pointer to the end of XML-description of the parent tag.
}

procedure TJvgXMLSerializer.DeSerializeInternal(Component: TObject;
  ComponentTagName: string; ParentBlockEnd: PChar = nil);
var
  BlockStart, BlockEnd, TagStart, TagEnd: PChar;
  TagName, TagValue, TagValueEnd: PChar;
  TypeInf: PTypeInfo;
  TypeData: PTypeData;
  PropIndex: Integer;
  AName: string;
  PropList: PPropList;
  NumProps: Word;

  //{ ����� � ������� �������� � �������� ������ }
  { Searching object for property with given name  [translated] }

  function FindProperty(TagName: PChar): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to NumProps - 1 do
      if CompareStr({$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropList^[I]^.Name), TagName) = 0 then
      begin
        Result := I;
        Break;
      end;
  end;

  procedure SkipSpaces(var TagEnd: PChar);
  begin
    while TagEnd[0] <= #33 do
      Inc(TagEnd);
  end;

begin
  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(TypeInf^.Name);
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  if not WrapCollections and (Component is TCollection) then
    ComponentTagName := TCollection(Component).ItemClass.ClassName;

  GetMem(PropList, NumProps * SizeOf(Pointer));
  try
    GetPropInfos(TypeInf, PropList);

    //{ ���� ����������� ��� }
    { Looking for opening tag  [translated] }
    BlockStart := StrPosExt(TokenPtr, PChar('<' + ComponentTagName + '>'),
      BufferEnd - TokenPtr { = BufferLength});

    //{ ���� ��� �� ������ � ��� ������� �������������, �� �� ������������ ��� }
    { If tag is not found and is not required - skip it  [translated] }
    if (BlockStart = nil) and not StrongConformity then
      exit;

    //{ ����� ��������� ��� ����������� }
    { Otherwise Check its presence  [translated] }
    Check(BlockStart <> nil, Format(RsOpenXMLTagNotFound,
      [ComponentTagName]), EJvgXMLOpenTagNotFoundException);
    Inc(BlockStart, Length(ComponentTagName) + 2);

    //{ ���� ����������� ��� }
    { Looking for closing tag  [translated] }
    BlockEnd := StrPosExt(BlockStart, PChar('</' + ComponentTagName + '>'),
      BufferEnd - BlockStart + 3 + Length(ComponentTagName) {BufferLength});
    Check(BlockEnd <> nil, Format(RsCloseXMLTagNotFound,
      [ComponentTagName]), EJvgXMLCloseTagNotFoundException);

    //{ �������� �� ��������� ����. ���� � ������������ ��� }
    { Checking the closing tag to be nested within parent tag  [translated] }
    Check((ParentBlockEnd = nil) or (BlockEnd < ParentBlockEnd),
      Format(RsCloseXMLTagNotFound, [ComponentTagName]),
      EJvgXMLCloseTagNotFoundException);

    TagEnd := BlockStart;
    SkipSpaces(TagEnd);

    //{ XML ������ }
    { XML parser [translated] }
    while (TagEnd < BlockEnd) { and (TagEnd >= TokenPtr)} do
    begin
      //{ ������� ����� ������� ������ }
      { fast search for "<" and ">"  [translated] }
      TagStart := TagEnd;
      while (TagStart^ <> '<') do
        Inc(TagStart);
      TagEnd := TagStart + 1;
      while (TagEnd^ <> '>') do
        Inc(TagEnd);

      GetMem(TagName, (TagEnd - TagStart + 1) * SizeOf(Char));
      try
        //{ TagName - ��� ���� }
        { Tag Name - Tag Name  [translated] }
        StrLCopy(TagName, TagStart + 1, TagEnd - TagStart - 1);

        //{ TagEnd - ����������� ��� }
        { TagEnd - Closing tag   [translated] }
        TagEnd := StrPosExt(TagEnd, PChar('</' + TagName + '>'),
          BufferEnd - TagEnd + 3 + Length(TagName) { = BufferLength});

        //Inc(TagStart, Length('</' + TagName + '>')-1);

        //{ ������ ���������� ��������� ���� }
        { Beginning of the next nested("children") tag [translated] }
        TagValue := TagStart + Length('</' + TagName + '>') - 1;
        TagValueEnd := TagEnd;

        //{ ����� ��������, ���������������� ���� }
        { Looking for property matching the tag  [translated] }
        PropIndex := FindProperty(TagName);

        if not WrapCollections and (PropIndex = -1) then
          PropIndex := FindProperty(PChar(string(TagName) + 's'))
        else
          TokenPtr := TagStart;

        if not IgnoreUnknownTags then
          Check(PropIndex <> -1, Format(RsUnknownProperty, [TagName]),
            EJvgXMLUnknownPropertyException);

        if PropIndex <> -1 then
          SetPropertyValue(Component, PropList^[PropIndex], TagValue,
            TagValueEnd, BlockEnd);

        Inc(TagEnd, Length('</' + TagName + '>'));
        SkipSpaces(TagEnd);
      finally
        FreeMem(TagName);
      end;
    end;
  finally
    FreeMem(PropList);//, NumProps * SizeOf(Pointer));
  end;
end;

//  ��������� ������������� �������� �������
//  ���������� ��:
//    DeSerializeInternal()
//  ����:
//    Component - ���������������� ������
//    PropInfo - ���������� � ���� ��� ���������������� ��������
//    Value - �������� ��������
//    ParentBlockEnd - ��������� �� ����� XML �������� ������������� ����
//                     ������������ ��� ��������

{
  Initialisation of the object's property
  Called from:
    DeSerializeInternal()
  Input:
    Component      - Component to be initialized
    PropInfo       - Information about type of property to set
    Value          - Value of the property
    ParentBlockEnd - Pointer to the end of XML description of parent tag. Used for recursion.
}

procedure TJvgXMLSerializer.SetPropertyValue(Component: TObject; PropInfo:
  PPropInfo; Value, ValueEnd: PChar; ParentBlockEnd: PChar);
var
  PropTypeInf: PTypeInfo;
  PropObject: TObject;
  CollectionItem: TCollectionItem;
  SValue: string;
  TmpChar: Char;
begin
  PropTypeInf := PropInfo.PropType^;

  case PropTypeInf^.Kind of
    tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
    {$IFDEF UNICODE} tkUString, {$ENDIF}
    tkWChar, tkLString, tkWString, tkVariant:
      begin
        //{ ��������� zero terminated string }
        { simulates zero terminated string  [translated] }
        TmpChar := ValueEnd[0];
        ValueEnd[0] := #0;
        SValue := StrPas(Value);
        ValueEnd[0] := TmpChar;

        // ������ ������������. ��������� ������ ��� XML,
        // ������������ � ������� ����� ����������
        { Replacing specific characters (compatible only with that very component)  [translated] }
        if FReplaceReservedSymbols then
        begin
          SValue := StringReplace(SValue, '&lt;', '<', [rfReplaceAll]);
          SValue := StringReplace(SValue, '&gt;', '>', [rfReplaceAll]);
          // SValue := StringReplace(SValue, '&', '&', [rfReplaceAll]);
        end;

        //{ ������ ����������� �� ��������� }
        { Changing delimiter to system-wide  [translated] }
        if PropTypeInf^.Kind = tkFloat then
          if JclFormatSettings.DecimalSeparator = ',' then
            SValue := StringReplace(SValue, '.', JclFormatSettings.DecimalSeparator, [rfReplaceAll])
          else
            SValue := StringReplace(SValue, ',', JclFormatSettings.DecimalSeparator, [rfReplaceAll]);

        //{ ��� ����������� �������������� �������� tkSet ����� ������� ������ }
        { tkSet parser needs "<" and ">" for correct transformation  [translated] }
        if PropTypeInf^.Kind = tkSet then
          SValue := '[' + SValue + ']';
        SetPropValue(Component, {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo^.Name), SValue);
      end;
    tkClass:
      begin
        PropObject := GetObjectProp(Component, PropInfo);
        if Assigned(PropObject) then
        begin
          //{ �������������� ������ � ��������� ������� }
          { Specific(individual) handling of some specific classes  [translated] }
          if PropObject is TStrings then
          //{ ��������� ������ }
          { text lists  [translated] }
          begin
            TmpChar := ValueEnd[0];
            ValueEnd[0] := #0;
            SValue := StrPas(Value);
            ValueEnd[0] := TmpChar;
            TStrings(PropObject).CommaText := SValue;
          end
          else
          if PropObject is TCollection then
          //{ ��������� }
          { collections  [translated] }
          begin
            while True do
            //{ ������� �� �������� ����� ��������� � ��������� }
            { we can't foretell number of element in TCollection  [translated] }
            begin
              CollectionItem := (PropObject as TCollection).Add;
              try
                DeSerializeInternal(CollectionItem,
                  CollectionItem.ClassName, ParentBlockEnd);
              except
                //{ ����������, ���� ��������� ������� �� ������ }
                { Exception if next element is not found  [translated] }
                on E: Exception do
                begin
                  // Application.MessageBox(PChar(E.Message), '', MB_OK); - debug string
                  CollectionItem.Free;
                  // raise;  - debug string
                  Break;
                end;
              end;
            end;
          end
          else
            //{ ��� ��������� ������� - ����������� ��������� }
            { Other classes are just processed recursevly  [translated] }
            DeSerializeInternal(PropObject, {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo^.Name),
              ParentBlockEnd);
        end;
      end;
  end;
end;

//  ��������� ��������� DTD ��� ��������� ������� �
//  ������������ � published ����������� ��� ������.
//  ����:
//    Component - ������
//  �����:
//    ����� DTD � ����� Stream

{
  This procedure generates DTD for given object according to its published interface
  Input:
    Component - Object
  Output:
    text of DTD into Stream
}

procedure TJvgXMLSerializer.GenerateDTD(Component: TObject; Stream: TStream);
var
  DTDList: TStringList;
begin
  DTDList := TStringList.Create;
  try
    GenerateDTDInternal(Component, DTDList, Stream, Component.ClassName);
  finally
    DTDList.Free;
  end;
end;

//  ���������� ����������� ��������� ��������� DTD ��� ��������� �������.
//  ����:
//    Component - ������
//    DTDList - ������ ��� ������������ ��������� DTD
//              ��� �������������� ����������.
//  �����:
//    ����� DTD � ����� Stream

{
  Inner recursive procedure that generates DTD for given object
  Input:
    Component - Object
    DTDList   - list of already determined describedDTD elements
                to avoid duplicating
  Output:
    DTD text into Stream
}

procedure TJvgXMLSerializer.GenerateDTDInternal(Component: TObject; DTDList:
  TStrings; Stream: TStream; const ComponentTagName: string);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  I: Integer;
  AName, PropName, TagContent: string;
  PropList: PPropList;
  NumProps: Word;
  PropObject: TObject;
const
  PCDATA = '#PCDATA';

  procedure addElement(const ElementName: string; Data: string);
  var
    S: string;
    UTF8S: UTF8String;
  begin
    if DTDList.IndexOf(ElementName) <> -1 then
      exit;
    DTDList.Add(ElementName);
    S := '<!ELEMENT ' + ElementName + ' ';
    if Data = '' then
      Data := PCDATA;
    S := S + '(' + Data + ')>'#13#10;
    UTF8S := UTF8Encode(S);
    Stream.Write(UTF8S[1], Length(UTF8S));
  end;

begin
  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(TypeInf^.Name);
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps * SizeOf(Pointer));
  try
    //{ �������� ������ ������� }
    { Getting list of properties  [translated] }
    GetPropInfos(TypeInf, PropList);
    TagContent := '';

    for I := 0 to NumProps - 1 do
    begin
      PropName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropList^[I]^.Name);

      PropTypeInf := PropList^[I]^.PropType^;
      PropInfo := PropList^[I];

      //{ ���������� �� �������������� ���� }
      { Skip types that are not supported [translated] }
      if not (PropTypeInf^.Kind in [tkDynArray, tkArray, tkRecord,
        tkInterface, tkMethod]) then
      begin
        if TagContent <> '' then
          TagContent := TagContent + '|';
        TagContent := TagContent + PropName;
      end;

      case PropTypeInf^.Kind of
        tkInteger, tkChar, tkFloat, tkString,
        {$IFDEF UNICODE} tkUString, {$ENDIF}
        tkWChar, tkLString, tkWString, tkVariant, tkEnumeration, tkSet:
          //{ ������� � DTD. ��� ������ ����� ������ ���������� - #PCDATA }
          { conversion to DTD. Theese types will have #PCDATA model of content [translated] }
          addElement(PropName, PCDATA);
        //{ ��� ��� �� ������� ��� ������������� ��������� }
        { Code might be useful when using attributes  [translated] }
        {
        tkEnumeration:
        begin
          TypeData:= GetTypeData(GetTypeData(PropTypeInf)^.BaseType^);
          S := '';
          for J := TypeData^.MinValue to TypeData^.MaxValue do
          begin
            if S <> '' then S := S + '|';
            S := S + GetEnumName(PropTypeInf, J);
          end;
          addElement(PropName, S);
        end;
        }
        tkClass:
          //{ ��� ��������� ����� ����������� ��������� }
          { make recursive call for class-types  [translated] }
          begin
            PropObject := GetObjectProp(Component, PropInfo);
            if Assigned(PropObject) then
            begin
              //{ ��� �������� �������-������� - ����������� ����� }
              { Specific(individual) handling of some specific classes [translated] }
              if PropObject is TPersistent then
                GenerateDTDInternal(PropObject, DTDList, Stream, PropName);
            end;
          end;
      end;
    end;

    //{ �������������� ������ � ��������� ������� }
    //{ ��� ��������� ���������� �������� � ������ ���������� ��� �������� }
    { Collections require item("element") type(class) to be included into
      content model [translated] }
    if Component is TCollection then
    begin
      if TagContent <> '' then
        TagContent := TagContent + '|';
      TagContent := TagContent + (Component as TCollection).ItemClass.ClassName + '*';
    end;

    //{ ��������� ������ ���������� ��� �������� }
    { Adding content model for the element(item)  [translated] }
    addElement(ComponentTagName, TagContent);
  finally
    FreeMem(PropList, NumProps * SizeOf(Pointer));
  end;
end;

procedure TJvgXMLSerializer.Check(Expr: Boolean; const Msg: string;
  E: TJvgXMLSerializerException);
begin
  if not Expr then
    raise E.Create('XMLSerializerException'#13#10#13#10 + Msg);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
