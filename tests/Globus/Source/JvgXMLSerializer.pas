{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgXMLSerializer.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgXMLSerializer;
{
  Компонент конвертирует компонент в XML и обратно в соответствии
  с published-интерфейсом класса компонента.

  XML формируется в виде пар тегов с вложенными в них значениями.
  Атрибуты у тегов отсутствуют.

  Тег верхнего уровня соответствует классу объекта.
  Вложенные теги соответствуют именам свойств.
  Для элементов коллекций контейнерный тег соответствует имени класса.

  Вложенность тегов не ограничена и полностью повторяет
  published интерфейс класса заданного объекта.

  Поддерживаются целые типы, типы с плавающей точкой, перечисления,
  наборы, строки, символы. вариантные типы,
  классовые типы, стоковые списки и коллекции.

  Интерфейс:
    procedure Serialize(Component: TObject; Stream: TStream);
     - Сериализация объекта в XML
    procedure DeSerialize(Component: TObject; Stream: TStream);
     - Загрузка XML в объект

    property GenerateFormattedXML       - создавать форматированный XML код
    property ExcludeEmptyValues         - пропускать пустые значения свойств
    property ExcludeDefaultValues       - пропускать значения по умолчанию
    property StrongConformity           - необходимо наличие в XML соотв. тегов для всех классовых типов
    property IgnoreUnknownTags          - игнорировать неизвестные теги при загрузке
    property OnGetXMLHeader             - позволяет указать свой XML заголовок

    WrapCollections - оборачивать коллекции в отдельные теги

  Ограничения:
    В в каждом объекте допустимо использовать только одну коллекцию каждого типа.

    Наследники класса TStrings не могут иметь published свойств.

    Процедурные типы не обрабатываются.

    Для генерации DTD у объекта все свойства классовых типов, одноименные со
    свойствами агрегированных объектов, должны быть одного класса.

  Предусловия:
    Объект для (де)сериализации должен быть создан до вызова процедуры.

    При StrongConformity == true необходимо присутствие в загружаемом XML тегов
    для всех классовых типов. Присутствие остальных тегов не проверяется.

  Дополнительно:
    При загрузке из XML содержимое коллекций в объекте не очищается,
    что позволяет дозагружать данные из множества источников в один объект.
}

interface

uses
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   comctrls,
   JVComponent,
   TypInfo;

resourcestring
   {$IFDEF RUSSIAN}
   ERR_OpenXMLTagNotFound     = 'Открывающий тег не найден: <%s>';
   ERR_CloseXMLTagNotFound    = 'Закрывающий тег не найден: </%s>';
   ERR_UncknownProperty       = 'Uncknown property: %s'
      {$ELSE}
   ERR_OpenXMLTagNotFound     = 'Open tag not found: <%s>';
   ERR_CloseXMLTagNotFound    = 'Close tag not found: </%s>';
   ERR_UncknownProperty       = 'Uncknown property: %s';
   {$ENDIF}

type
   TOnGetXMLHeader = procedure(Sender: TObject; var Value: string) of object;
   TBeforeParsingEvent = procedure(Sender: TObject; Buffer: PChar) of object;

   EJvgXMLSerializerException = class(Exception)
   end;

   XMLSerializerException = class(Exception)
   end;
   EJvgXMLOpenTagNotFoundException = class(XMLSerializerException)
   end;
   EJvgXMLCloseTagNotFoundException = class(XMLSerializerException)
   end;
   EJvgXMLUncknownPropertyException = class(XMLSerializerException)
   end;

   TJvgXMLSerializerException = class of XMLSerializerException;

   TJvgXMLSerializer = class(TJvComponent)
   private
      Buffer: PChar;
      BufferEnd: PChar;
      BufferLength: DWORD;
      TokenPtr {, MaxTokenPtr}: PChar;
      OutStream: TStream;

      FOnGetXMLHeader: TOnGetXMLHeader;
      FGenerateFormattedXML: boolean;
      FExcludeEmptyValues: boolean;
      FExcludeDefaultValues: boolean;
      FReplaceReservedSymbols: boolean;
      FStrongConformity: boolean;
      FBeforeParsing: TBeforeParsingEvent;
      FWrapCollections: boolean;
      FIgnoreUnknownTags: boolean;
      procedure check(Expr: boolean; const Message: string; E:
         TJvgXMLSerializerException);

      procedure WriteOutStream(Value: string);
      { Private declarations }
   protected
      procedure SerializeInternal(Component: TObject; Level: integer = 1);
      procedure DeSerializeInternal(Component: TObject; {const}
         ComponentTagName: string; ParentBlockEnd: PChar = nil);
      procedure GenerateDTDInternal(Component: TObject; DTDList: TStrings;
         Stream: TStream; const ComponentTagName: string);
      procedure SetPropertyValue(Component: TObject; PropInfo: PPropInfo; Value,
         ValueEnd: PChar; ParentBlockEnd: PChar);
   public
      DefaultXMLHeader: string;
      tickCounter, tickCount: DWORD;
      constructor Create(AOwner: TComponent); override;
      { Сериализация объекта в XML }
      procedure Serialize(Component: TObject; Stream: TStream);
      { Загрузка XML в объект }
      procedure DeSerialize(Component: TObject; Stream: TStream);
      { Генерация DTD }
      procedure GenerateDTD(Component: TObject; Stream: TStream);
   published
      property GenerateFormattedXML: boolean
         read FGenerateFormattedXML write FGenerateFormattedXML default true;
      property ExcludeEmptyValues: boolean
         read FExcludeEmptyValues write FExcludeEmptyValues;
      property ExcludeDefaultValues: boolean
         read FExcludeDefaultValues write FExcludeDefaultValues;
      property ReplaceReservedSymbols: boolean
         read FReplaceReservedSymbols write FReplaceReservedSymbols;
      property StrongConformity: boolean
         read FStrongConformity write FStrongConformity default true;
      property IgnoreUnknownTags: boolean
         read FIgnoreUnknownTags write FIgnoreUnknownTags;

      property WrapCollections: boolean
         read FWrapCollections write FWrapCollections default true;

      property OnGetXMLHeader: TOnGetXMLHeader
         read FOnGetXMLHeader write FOnGetXMLHeader;
      property BeforeParsing: TBeforeParsingEvent
         read FBeforeParsing write FBeforeParsing;
   end;

procedure Register;

implementation
uses JvgUtils
   //mb {$IFDEF COMPILER6_UP},
   //mb  DesignIntf{$ELSE}{$IFDEF COMPILER5_UP},
   //mb  dsgnintf{$ENDIF}{$ENDIF}
   ;

const
   ORDINAL_TYPES              = [tkInteger, tkChar, tkEnumeration, tkSet];
var
   TAB                        : string;
   CR                         : string;

procedure Register;
begin
end;

constructor TJvgXMLSerializer.Create(AOwner: TComponent);
begin
   inherited;
   //...defaults
   FGenerateFormattedXML := true;
   FStrongConformity := true;
   FWrapCollections := true;
end;

{ пишет строку в выходящий поток. Исп-ся при сериализации }

procedure TJvgXMLSerializer.WriteOutStream(Value: string);
begin
   OutStream.Write(Pchar(Value)[0], Length(Value));
end;

{
  Конвертирует компонент в XML-код в соответствии
  с published интерфейсом класса объекта.
  Вход:
    Component - компонент для конвертации
  Выход:
    текст XML в поток Stream
}

procedure TJvgXMLSerializer.Serialize(Component: TObject; Stream: TStream);
var
   Result                     : string;
begin
   TAB := IIF(GenerateFormattedXML, #9, '');
   CR := IIF(GenerateFormattedXML, #13#10, '');

   Result := '';
   { Получение XML заголовка }
   if Assigned(OnGetXMLHeader) then
      OnGetXMLHeader(self, Result);
   if Result = '' then
      Result := DefaultXMLHeader;

   OutStream := Stream;

   WriteOutStream(PChar(Result));

   WriteOutStream(PChar(CR + '<' + Component.ClassName + '>'));
   SerializeInternal(Component);
   WriteOutStream(PChar(CR + '</' + Component.ClassName + '>'));
end;

{
  Внутренняя процедура конвертации объекта в XML
  Вызывается из:
    Serialize()
  Вход:
    Component - компонент для конвертации
    Level - уровень вложенности тега для форматирования результата
  Выход:
    строка XML в выходной поток через метод WriteOutStream()
}

procedure TJvgXMLSerializer.SerializeInternal(Component: TObject; Level: integer
   = 1);
var
   PropInfo                   : PPropInfo;
   TypeInf, PropTypeInf       : PTypeInfo;
   TypeData                   : PTypeData;
   i, j                       : integer;
   AName, PropName, sPropValue: string;
   PropList                   : PPropList;
   NumProps                   : word;
   PropObject                 : TObject;

   { Добавляет открывающий тег с заданным именем }

   procedure addOpenTag(const Value: string);
   begin
      WriteOutStream(CR + DupStr(TAB, Level) + '<' + Value + '>');
      inc(Level);
   end;

   { Добавляет закрывающий тег с заданным именем }

   procedure addCloseTag(const Value: string; addBreak: boolean = false);
   begin
      dec(Level);
      if addBreak then
         WriteOutStream(CR + DupStr(TAB, Level));
      WriteOutStream('</' + Value + '>');
   end;

   { Добавляет значение в результирующую строку }

   procedure addValue(const Value: string);
   begin
      WriteOutStream(Value);
   end;
begin
   //  Result := '';

     { Playing with RTTI }
   TypeInf := Component.ClassInfo;
   AName := TypeInf^.Name;
   TypeData := GetTypeData(TypeInf);
   NumProps := TypeData^.PropCount;

   GetMem(PropList, NumProps * sizeof(pointer));
   try

      { Получаем список свойств }
      GetPropInfos(TypeInf, PropList);

      for i := 0 to NumProps - 1 do
      begin
         PropName := PropList^[i]^.Name;

         PropTypeInf := PropList^[i]^.PropType^;
         PropInfo := PropList^[i];

         { Хочет ли свойство, чтобы его сохранили ? }
         if not IsStoredProp(Component, PropInfo) then
            continue;

         case PropTypeInf^.Kind of
            tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
               tkWChar, tkLString, tkWString, tkVariant:
               begin
                  { Получение значения свойства }
                  sPropValue := GetPropValue(Component, PropName, true);

                  { Проверяем на пустое значение и значение по умолчанию }
                  if ExcludeEmptyValues and (sPropValue = '') then
                     continue;
                  if ExcludeDefaultValues and (PropTypeInf^.Kind in
                     ORDINAL_TYPES)
                     and (sPropValue = IntToStr(PropInfo.Default)) then
                     continue;

                  { Замена спецсимволов }
                  if FReplaceReservedSymbols then
                  begin
                     sPropValue := StringReplace(sPropValue, '<', '&lt;',
                        [rfReplaceAll]);
                     sPropValue := StringReplace(sPropValue, '>', '&gt;',
                        [rfReplaceAll]);
                     // sPropValue := StringReplace(sPropValue, '&', '&', [rfReplaceAll]);
                  end;

                  { Перевод в XML }
                  addOpenTag(PropName);
                  addValue(sPropValue); { Добавляем значение свойства в результат }
                  addCloseTag(PropName);
               end;
            tkClass: { Для классовых типов рекурсивная обработка }
               begin

                  PropObject := GetObjectProp(Component, PropInfo);
                  if Assigned(PropObject) then
                  begin
                     { Для дочерних свойств-классов - рекурсивный вызов }

                     { Индивидуальный подход к некоторым классам }
                     if (PropObject is TStrings) then { Текстовые списки }
                     begin
                        addOpenTag(PropName);
                        WriteOutStream(TStrings(PropObject).CommaText);
                        addCloseTag(PropName, true);
                     end
                     else if (PropObject is TCollection) then { Коллекции }
                     begin
                        if WrapCollections then
                           addOpenTag(PropName);

                        SerializeInternal(PropObject, Level);
                        for j := 0 to (PropObject as TCollection).Count - 1 do
                        begin           { Контейнерный тег по имени класса }
                           addOpenTag(TCollection(PropObject).Items[j].ClassName);
                           SerializeInternal(TCollection(PropObject).Items[j],
                              Level);
                           addCloseTag(TCollection(PropObject).Items[j].ClassName, true);
                        end;

                        if WrapCollections then
                           addCloseTag(PropName, true);
                     end
                     else if (PropObject is TPersistent) then
                     begin
                        addOpenTag(PropName);
                        SerializeInternal(PropObject, Level);
                        addCloseTag(PropName, true);
                     end;

                     { Здесь можно добавить обработку остальных классов: TTreeNodes, TListItems }
                  end;
                  { После обработки свойств закрываем тег объекта }

               end;

         end;
      end;
   finally
      FreeMem(PropList, NumProps * sizeof(pointer));
   end;

end;

{
  Загружает в компонент данные из потока с XML-кодом.
  Вход:
    Component - компонент для конвертации
    Stream - источник загрузки XML
  Предусловия:
    Объект Component должен быть создан до вызова процедуры
}

procedure TJvgXMLSerializer.DeSerialize(Component: TObject; Stream: TStream);
begin
   GetMem(Buffer, Stream.Size);
   try
      { Получаем данные из потока }
      Stream.Read(Buffer[0], Stream.Size + 1);

      if Assigned(BeforeParsing) then
         BeforeParsing(self, Buffer);

      { Устанавливаем текущий указатель чтения данных }
      TokenPtr := Buffer;
      BufferLength := Stream.Size - 1;
      BufferEnd := Buffer + BufferLength;
      { Вызываем загрузчик }
      DeSerializeInternal(Component, Component.ClassName);
   finally
      FreeMem(Buffer);
   end;
end;

{
  Рекурсивная процедура загрузки объекта их текстового буфера с XML
  Вызывается из:
    Serialize()
  Вход:
    Component - компонент для конвертации
    ComponentTagName - имя XML тега объекта
    ParentBlockEnd - указатель на конец XML описания родительского тега
}

procedure TJvgXMLSerializer.DeSerializeInternal(Component: TObject; {const}
   ComponentTagName: string; ParentBlockEnd: PChar = nil);
var
   BlockStart, BlockEnd, TagStart, TagEnd: PChar;
   TagName, TagValue, TagValueEnd: PChar;
   TypeInf                    : PTypeInfo;
   TypeData                   : PTypeData;
   PropIndex                  : integer;
   AName                      : string;
   PropList                   : PPropList;
   NumProps                   : word;

   { Поиск у объекта свойства с заданным именем }
   function FindProperty(TagName: PChar): integer;
   var
      i                       : integer;
   begin
      Result := -1;
      for i := 0 to NumProps - 1 do
         if CompareStr(PropList^[i]^.Name, TagName) = 0 then
         begin
            Result := i;
            break;
         end;
   end;

   procedure SkipSpaces(var TagEnd: PChar);
   begin
      while TagEnd[0] <= #33 do
         inc(TagEnd);
   end;

   {
     StrPosExt - ищет позицию одной строки в другой с заданной длиной.
     На длинных строках превосходит StrPos.
   }
   function StrPosExt(const Str1, Str2: PChar; Str1Len: DWORD): PChar;
      assembler;
   asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX         // Str1
        JE      @@2             // если строка Str1 пуста - на выход
        OR      EDX,EDX         // Str2
        JE      @@2             // если строка Str2 пуста - на выход
        MOV     EBX,EAX
        MOV     EDI,EDX         // установим смещение для SCASB - подстрока Str2
        XOR     AL,AL           // обнулим AL

        push ECX                // длина строки

        MOV     ECX,0FFFFFFFFH  // счетчик с запасом
        REPNE   SCASB           // ищем конец подстроки Str2
        NOT     ECX             // инвертируем ECX - получаем длину строки+1
        DEC     ECX             // в ECX - длина искомой подстроки Str2

        JE      @@2             // при нулевой длине - все на выход
        MOV     ESI,ECX         // сохраняем длину подстроки в ESI

        pop ECX

        SUB     ECX,ESI         // ECX == разница длин строк : Str1 - Str2
        JBE     @@2             // если длина подсроки больше длине строки - выход
        MOV     EDI,EBX         // EDI  - начало строки Str1
        LEA     EBX,[ESI-1]     // EBX - длина сравнения строк
@@1:    MOV     ESI,EDX         // ESI - смещение строки Str2
        LODSB                   // загужаем первый символ подстроки в AL
        REPNE   SCASB           // ищем этот символ в строке EDI
        JNE     @@2             // если символ не обнаружен - на выход
        MOV     EAX,ECX         // сохраним разницу длин строк
        PUSH    EDI             // запомним текущее смещение поиска
        MOV     ECX,EBX
        REPE    CMPSB           // побайтно сравниваем строки
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1             // если строки различны - ищем следующее совпадение первого символа
        LEA     EAX,[EDI-1]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
   end;

begin
   { Playing with RTTI }
   TypeInf := Component.ClassInfo;
   AName := TypeInf^.Name;
   TypeData := GetTypeData(TypeInf);
   NumProps := TypeData^.PropCount;

   GetMem(PropList, NumProps * sizeof(pointer));

   if not WrapCollections and (Component is TCollection) then
      ComponentTagName := TCollection(Component).ItemClass.ClassName;

   try
      GetPropInfos(TypeInf, PropList);

      { ищем открывающий тег }
      BlockStart := StrPosExt(TokenPtr, PChar('<' + ComponentTagName + '>'),
         BufferEnd - TokenPtr { = BufferLength});

      { Если тег не найден и его наличие необязательно, то не обрабатываем его }
      if (BlockStart = nil) and not StrongConformity then
         exit;

      { иначе проверяем его присутствие }
      check(BlockStart <> nil, Format(ERR_OpenXMLTagNotFound,
         [ComponentTagName]), EJvgXMLOpenTagNotFoundException);
      inc(BlockStart, length(ComponentTagName) + 2);

      { ищем закрывающий тег }
      BlockEnd := StrPosExt(BlockStart, PChar('</' + ComponentTagName + '>'),
         BufferEnd - BlockStart + 3 + length(ComponentTagName) {BufferLength});
      check(BlockEnd <> nil, Format(ERR_CloseXMLTagNotFound,
         [ComponentTagName]), EJvgXMLCloseTagNotFoundException);

      { проверка на вхождение закр. тега в родительский тег }
      check((ParentBlockEnd = nil) or (BlockEnd < ParentBlockEnd),
         Format(ERR_CloseXMLTagNotFound, [ComponentTagName]),
         EJvgXMLCloseTagNotFoundException);

      TagEnd := BlockStart;
      SkipSpaces(TagEnd);

      { XML парсер }
      while (TagEnd < BlockEnd) { and (TagEnd >= TokenPtr)} do
      begin
         { быстрый поиск угловых скобок }
         asm
      mov CL, '<'
      mov EDX, Pointer(TagEnd)
      dec EDX
@@1:  inc EDX
      mov AL, byte[EDX]
      cmp AL, CL
      jne @@1
      mov TagStart, EDX

      mov CL, '>'
@@2:  inc EDX
      mov AL, byte[EDX]
      cmp AL, CL
      jne @@2
      mov TagEnd, EDX
         end;

         GetMem(TagName, TagEnd - TagStart + 1);
         try

            { TagName - имя тега }
            StrLCopy(TagName, TagStart + 1, TagEnd - TagStart - 1);

            { TagEnd - закрывающий тег }
            TagEnd := StrPosExt(TagEnd, PChar('</' + TagName + '>'), BufferEnd -
               TagEnd + 3 + length(TagName) { = BufferLength});

            //inc(TagStart, length('</' + TagName + '>')-1);

            { начало очередного дочернего тега }
            TagValue := TagStart + length('</' + TagName + '>') - 1;
            TagValueEnd := TagEnd;

            { поиск свойства, соответствующего тегу }
            PropIndex := FindProperty(TagName);

            if not WrapCollections and (PropIndex = -1) then
            begin
               PropIndex := FindProperty(Pchar(TagName + 's'));

            end
            else
               TokenPtr := TagStart;

            if not IgnoreUnknownTags then
               check(PropIndex <> -1, Format(ERR_UncknownProperty, [TagName]),
                  EJvgXMLUncknownPropertyException);

            if PropIndex <> -1 then
               SetPropertyValue(Component, PropList^[PropIndex], TagValue,
                  TagValueEnd, BlockEnd);

            inc(TagEnd, length('</' + TagName + '>'));
            SkipSpaces(TagEnd);

         finally
            FreeMem(TagName);
         end;

      end;

   finally
      FreeMem(PropList, NumProps * sizeof(pointer));
   end;

end;

{
  Процедура инициализации свойства объекта
  Вызывается из:
    DeSerializeInternal()
  Вход:
    Component - инициализируемый объект
    PropInfo - информация о типе для устанавливаемого свойства
    Value - значение свойства
    ParentBlockEnd - указатель на конец XML описания родительского тега
                     Используется для рекурсии
}

procedure TJvgXMLSerializer.SetPropertyValue(Component: TObject; PropInfo:
   PPropInfo; Value, ValueEnd: PChar; ParentBlockEnd: PChar);
var
   PropTypeInf                : PTypeInfo;
   PropObject                 : TObject;
   CollectionItem             : TCollectionItem;
   sValue                     : string;
   charTmp                    : char;
begin
   PropTypeInf := PropInfo.PropType^;

   case PropTypeInf^.Kind of
      tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
         tkWChar, tkLString, tkWString, tkVariant:
         begin
            { имитируем zero terminated string }
            charTmp := ValueEnd[0];
            ValueEnd[0] := #0;
            sValue := StrPas(Value);
            ValueEnd[0] := charTmp;

            { Замена спецсимволов. Актуально только для XML,
             сохраненного с помощью этого компонента }
            if FReplaceReservedSymbols then
            begin
               sValue := StringReplace(sValue, '&lt;', '<', [rfReplaceAll]);
               sValue := StringReplace(sValue, '&gt;', '>', [rfReplaceAll]);
               // sValue := StringReplace(sValue, '&', '&', [rfReplaceAll]);
            end;

            { Замена разделителя на системный }
            if PropTypeInf^.Kind = tkFloat then
            begin
               if DecimalSeparator = ',' then
                  sValue := StringReplace(sValue, '.', DecimalSeparator,
                     [rfReplaceAll])
               else
                  sValue := StringReplace(sValue, ',', DecimalSeparator,
                     [rfReplaceAll]);
            end;

            { Для корректного преобразования парсером tkSet нужны угловые скобки }
            if PropTypeInf^.Kind = tkSet then
               sValue := '[' + sValue + ']';
            SetPropValue(Component, PropInfo^.Name, sValue);
         end;
      tkClass:
         begin
            PropObject := GetObjectProp(Component, PropInfo);
            if Assigned(PropObject) then
            begin
               { Индивидуальный подход к некоторым классам }
               if (PropObject is TStrings) then { Текстовые списки }
               begin
                  charTmp := ValueEnd[0];
                  ValueEnd[0] := #0;
                  sValue := StrPas(Value);
                  ValueEnd[0] := charTmp;
                  TStrings(PropObject).CommaText := sValue;
               end
               else if (PropObject is TCollection) then { Коллекции }
               begin
                  while true do { Заранее не известно число элементов в коллекции }
                  begin
                     CollectionItem := (PropObject as TCollection).Add;
                     try
                        DeSerializeInternal(CollectionItem,
                           CollectionItem.ClassName, ParentBlockEnd);
                     except { Исключение, если очередной элемент не найден }
                        on E: Exception do
                        begin
                           // Application.MessageBox(PChar(E.Message), '', MB_OK); - debug string
                           CollectionItem.Free;
                           // raise;  - debug string
                           break;
                        end;
                     end;
                  end;
               end
               else { Для остальных классов - рекурсивная обработка }
                  DeSerializeInternal(PropObject, PropInfo^.Name,
                     ParentBlockEnd);
            end;
         end;
   end;
end;

{
  Процедура генерации DTD для заданного объекта в
  соответствии с published интерфейсом его класса.
  Вход:
    Component - объект
  Выход:
    текст DTD в поток Stream
}

procedure TJvgXMLSerializer.GenerateDTD(Component: TObject; Stream: TStream);
var
   DTDList                    : TStringList;
begin
   DTDList := TStringList.Create;
   try
      GenerateDTDInternal(Component, DTDList, Stream, Component.ClassName);
   finally
      DTDList.Free;
   end;
end;

{
  Внутренняя рекурсивная процедура генерации DTD для заданного объекта.
  Вход:
    Component - объект
    DTDList - список уже определенных элементов DTD
              для предотвращения повторений.
  Выход:
    текст DTD в поток Stream
}

procedure TJvgXMLSerializer.GenerateDTDInternal(Component: TObject; DTDList:
   TStrings; Stream: TStream; const ComponentTagName: string);
var
   PropInfo                   : PPropInfo;
   TypeInf, PropTypeInf       : PTypeInfo;
   TypeData                   : PTypeData;
   i                          : integer;
   AName, PropName, TagContent: string;
   PropList                   : PPropList;
   NumProps                   : word;
   PropObject                 : TObject;
const
   PCDATA                     = '#PCDATA';

   procedure addElement(const ElementName: string; Data: string);
   var
      s                       : string;
   begin
      if DTDList.IndexOf(ElementName) <> -1 then
         exit;
      DTDList.Add(ElementName);
      s := '<!ELEMENT ' + ElementName + ' ';
      if Data = '' then
         Data := PCDATA;
      s := s + '(' + Data + ')>'#13#10;
      Stream.Write(PChar(s)[0], length(s));
   end;
begin
   { Playing with RTTI }
   TypeInf := Component.ClassInfo;
   AName := TypeInf^.Name;
   TypeData := GetTypeData(TypeInf);
   NumProps := TypeData^.PropCount;

   GetMem(PropList, NumProps * sizeof(pointer));
   try
      { Получаем список свойств }
      GetPropInfos(TypeInf, PropList);
      TagContent := '';

      for i := 0 to NumProps - 1 do
      begin
         PropName := PropList^[i]^.Name;

         PropTypeInf := PropList^[i]^.PropType^;
         PropInfo := PropList^[i];

         { Пропустить не поддерживаемые типы }
         if not (PropTypeInf^.Kind in [tkDynArray, tkArray, tkRecord,
            tkInterface, tkMethod]) then
         begin
            if TagContent <> '' then
               TagContent := TagContent + '|';
            TagContent := TagContent + PropName;
         end;

         case PropTypeInf^.Kind of
            tkInteger, tkChar, tkFloat, tkString,
               tkWChar, tkLString, tkWString, tkVariant, tkEnumeration, tkSet:
               begin
                  { Перевод в DTD. Для данных типов модель содержания - #PCDATA }
                  addElement(PropName, PCDATA);
               end;
            { код был бы полезен при использовании атрибутов
            tkEnumeration:
            begin
              TypeData:= GetTypeData(GetTypeData(PropTypeInf)^.BaseType^);
              s := '';
              for j := TypeData^.MinValue to TypeData^.MaxValue do
              begin
                if s <> '' then s := s + '|';
                s := s + GetEnumName(PropTypeInf, j);
              end;
              addElement(PropName, s);
            end;
            }
            tkClass: { Для классовых типов рекурсивная обработка }
               begin
                  PropObject := GetObjectProp(Component, PropInfo);
                  if Assigned(PropObject) then
                  begin
                     { Для дочерних свойств-классов - рекурсивный вызов }
                     if (PropObject is TPersistent) then
                        GenerateDTDInternal(PropObject, DTDList, Stream,
                           PropName);
                  end;
               end;
         end;
      end;

      { Индивидуальный подход к некоторым классам }
      { Для коллекций необходимо включить в модель содержания тип элемента }
      if (Component is TCollection) then
      begin
         if TagContent <> '' then
            TagContent := TagContent + '|';
         TagContent := TagContent + (Component as
            TCollection).ItemClass.ClassName + '*';
      end;

      { Добавляем модель содержания для элемента }
      addElement(ComponentTagName, TagContent);
   finally
      FreeMem(PropList, NumProps * sizeof(pointer));
   end;
end;

procedure TJvgXMLSerializer.check(Expr: boolean; const Message: string; E:
   TJvgXMLSerializerException);

begin
   if not Expr then
      raise E.Create('XMLSerializerException'#13#10#13#10 + Message);
end;

end.

