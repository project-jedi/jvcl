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

UNIT JvgXMLSerializer;
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

INTERFACE

USES
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

TYPE
   TOnGetXMLHeader = PROCEDURE(Sender: TObject; VAR Value: STRING) OF OBJECT;
   TBeforeParsingEvent = PROCEDURE(Sender: TObject; Buffer: PChar) OF OBJECT;

   EJvgXMLSerializerException = CLASS(Exception)
   END;

   TJvgXMLSerializer = CLASS(TJvComponent)
   PRIVATE
      Buffer: PChar;
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
      PROCEDURE check(Expr: boolean; CONST Message: STRING);
      PROCEDURE WriteOutStream(Value: STRING);
      { Private declarations }
   PROTECTED
      PROCEDURE SerializeInternal(Component: TObject; Level: integer = 1);
      PROCEDURE DeSerializeInternal(Component: TObject; {const}
         ComponentTagName: STRING; ParentBlockEnd: PChar = NIL);
      PROCEDURE GenerateDTDInternal(Component: TObject; DTDList: TStrings;
         Stream: TStream; CONST ComponentTagName: STRING);
      PROCEDURE SetPropertyValue(Component: TObject; PropInfo: PPropInfo; Value,
         ValueEnd: PChar; ParentBlockEnd: PChar);
   PUBLIC
      DefaultXMLHeader: STRING;
      tickCounter, tickCount: DWORD;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      { Сериализация объекта в XML }
      PROCEDURE Serialize(Component: TObject; Stream: TStream);
      { Загрузка XML в объект }
      PROCEDURE DeSerialize(Component: TObject; Stream: TStream);
      { Генерация DTD }
      PROCEDURE GenerateDTD(Component: TObject; Stream: TStream);
   PUBLISHED
      PROPERTY GenerateFormattedXML: boolean
         READ FGenerateFormattedXML WRITE FGenerateFormattedXML DEFAULT true;
      PROPERTY ExcludeEmptyValues: boolean
         READ FExcludeEmptyValues WRITE FExcludeEmptyValues;
      PROPERTY ExcludeDefaultValues: boolean
         READ FExcludeDefaultValues WRITE FExcludeDefaultValues;
      PROPERTY ReplaceReservedSymbols: boolean
         READ FReplaceReservedSymbols WRITE FReplaceReservedSymbols;
      PROPERTY StrongConformity: boolean
         READ FStrongConformity WRITE FStrongConformity DEFAULT true;
      PROPERTY IgnoreUnknownTags: boolean
         READ FIgnoreUnknownTags WRITE FIgnoreUnknownTags;

      PROPERTY WrapCollections: boolean
         READ FWrapCollections WRITE FWrapCollections DEFAULT true;

      PROPERTY OnGetXMLHeader: TOnGetXMLHeader
         READ FOnGetXMLHeader WRITE FOnGetXMLHeader;
      PROPERTY BeforeParsing: TBeforeParsingEvent
         READ FBeforeParsing WRITE FBeforeParsing;
   END;

PROCEDURE Register;

IMPLEMENTATION
USES JvgUtils{$IFDEF COMPILER6_UP},
   DesignIntf{$ELSE}{$IFDEF COMPILER5_UP},
   dsgnintf{$ENDIF}{$ENDIF};

CONST
   ORDINAL_TYPES              = [tkInteger, tkChar, tkEnumeration, tkSet];
VAR
   TAB                        : STRING;
   CR                         : STRING;

PROCEDURE Register;
BEGIN
END;

CONSTRUCTOR TJvgXMLSerializer.Create(AOwner: TComponent);
BEGIN
   INHERITED;
   //...defaults
   FGenerateFormattedXML := true;
   FStrongConformity := true;
   FWrapCollections := true;
END;

{ пишет строку в выходящий поток. Исп-ся при сериализации }

PROCEDURE TJvgXMLSerializer.WriteOutStream(Value: STRING);
BEGIN
   OutStream.Write(Pchar(Value)[0], Length(Value));
END;

{
  Конвертирует компонент в XML-код в соответствии
  с published интерфейсом класса объекта.
  Вход:
    Component - компонент для конвертации
  Выход:
    текст XML в поток Stream
}

PROCEDURE TJvgXMLSerializer.Serialize(Component: TObject; Stream: TStream);
VAR
   Result                     : STRING;
BEGIN
   TAB := IIF(GenerateFormattedXML, #9, '');
   CR := IIF(GenerateFormattedXML, #13#10, '');

   Result := '';
   { Получение XML заголовка }
   IF Assigned(OnGetXMLHeader) THEN
      OnGetXMLHeader(self, Result);
   IF Result = '' THEN
      Result := DefaultXMLHeader;

   OutStream := Stream;

   WriteOutStream(PChar(Result));

   WriteOutStream(PChar(CR + '<' + Component.ClassName + '>'));
   SerializeInternal(Component);
   WriteOutStream(PChar(CR + '</' + Component.ClassName + '>'));
END;

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

PROCEDURE TJvgXMLSerializer.SerializeInternal(Component: TObject; Level: integer
   = 1);
VAR
   PropInfo                   : PPropInfo;
   TypeInf, PropTypeInf       : PTypeInfo;
   TypeData                   : PTypeData;
   i, j                       : integer;
   AName, PropName, sPropValue: STRING;
   PropList                   : PPropList;
   NumProps                   : word;
   PropObject                 : TObject;

   { Добавляет открывающий тег с заданным именем }

   PROCEDURE addOpenTag(CONST Value: STRING);
   BEGIN
      WriteOutStream(CR + DupStr(TAB, Level) + '<' + Value + '>');
      inc(Level);
   END;

   { Добавляет закрывающий тег с заданным именем }

   PROCEDURE addCloseTag(CONST Value: STRING; addBreak: boolean = false);
   BEGIN
      dec(Level);
      IF addBreak THEN
         WriteOutStream(CR + DupStr(TAB, Level));
      WriteOutStream('</' + Value + '>');
   END;

   { Добавляет значение в результирующую строку }

   PROCEDURE addValue(CONST Value: STRING);
   BEGIN
      WriteOutStream(Value);
   END;
BEGIN
   //  Result := '';

     { Playing with RTTI }
   TypeInf := Component.ClassInfo;
   AName := TypeInf^.Name;
   TypeData := GetTypeData(TypeInf);
   NumProps := TypeData^.PropCount;

   GetMem(PropList, NumProps * sizeof(pointer));
   TRY

      { Получаем список свойств }
      GetPropInfos(TypeInf, PropList);

      FOR i := 0 TO NumProps - 1 DO
      BEGIN
         PropName := PropList^[i]^.Name;

         PropTypeInf := PropList^[i]^.PropType^;
         PropInfo := PropList^[i];

         { Хочет ли свойство, чтобы его сохранили ? }
         IF NOT IsStoredProp(Component, PropInfo) THEN
            continue;

         CASE PropTypeInf^.Kind OF
            tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
               tkWChar, tkLString, tkWString, tkVariant:
               BEGIN
                  { Получение значения свойства }
                  sPropValue := GetPropValue(Component, PropName, true);

                  { Проверяем на пустое значение и значение по умолчанию }
                  IF ExcludeEmptyValues AND (sPropValue = '') THEN
                     continue;
                  IF ExcludeDefaultValues AND (PropTypeInf^.Kind IN
                     ORDINAL_TYPES)
                     AND (sPropValue = IntToStr(PropInfo.Default)) THEN
                     continue;

                  { Замена спецсимволов }
                  IF FReplaceReservedSymbols THEN
                  BEGIN
                     sPropValue := StringReplace(sPropValue, '<', '&lt;',
                        [rfReplaceAll]);
                     sPropValue := StringReplace(sPropValue, '>', '&gt;',
                        [rfReplaceAll]);
                     // sPropValue := StringReplace(sPropValue, '&', '&', [rfReplaceAll]);
                  END;

                  { Перевод в XML }
                  addOpenTag(PropName);
                  addValue(sPropValue); { Добавляем значение свойства в результат }
                  addCloseTag(PropName);
               END;
            tkClass: { Для классовых типов рекурсивная обработка }
               BEGIN

                  PropObject := GetObjectProp(Component, PropInfo);
                  IF Assigned(PropObject) THEN
                  BEGIN
                     { Для дочерних свойств-классов - рекурсивный вызов }

                     { Индивидуальный подход к некоторым классам }
                     IF (PropObject IS TStrings) THEN { Текстовые списки }
                     BEGIN
                        addOpenTag(PropName);
                        WriteOutStream(TStrings(PropObject).CommaText);
                        addCloseTag(PropName, true);
                     END
                     ELSE IF (PropObject IS TCollection) THEN { Коллекции }
                     BEGIN
                        IF WrapCollections THEN
                           addOpenTag(PropName);

                        SerializeInternal(PropObject, Level);
                        FOR j := 0 TO (PropObject AS TCollection).Count - 1 DO
                        BEGIN           { Контейнерный тег по имени класса }
                           addOpenTag(TCollection(PropObject).Items[j].ClassName);
                           SerializeInternal(TCollection(PropObject).Items[j],
                              Level);
                           addCloseTag(TCollection(PropObject).Items[j].ClassName, true);
                        END;

                        IF WrapCollections THEN
                           addCloseTag(PropName, true);
                     END
                     ELSE IF (PropObject IS TPersistent) THEN
                     BEGIN
                        addOpenTag(PropName);
                        SerializeInternal(PropObject, Level);
                        addCloseTag(PropName, true);
                     END;

                     { Здесь можно добавить обработку остальных классов: TTreeNodes, TListItems }
                  END;
                  { После обработки свойств закрываем тег объекта }

               END;

         END;
      END;
   FINALLY
      FreeMem(PropList, NumProps * sizeof(pointer));
   END;

END;

{
  Загружает в компонент данные из потока с XML-кодом.
  Вход:
    Component - компонент для конвертации
    Stream - источник загрузки XML
  Предусловия:
    Объект Component должен быть создан до вызова процедуры
}

PROCEDURE TJvgXMLSerializer.DeSerialize(Component: TObject; Stream: TStream);
BEGIN
   GetMem(Buffer, Stream.Size);
   TRY
      { Получаем данные из потока }
      Stream.Read(Buffer[0], Stream.Size + 1);

      IF Assigned(BeforeParsing) THEN
         BeforeParsing(self, Buffer);

      { Устанавливаем текущий указатель чтения данных }
      TokenPtr := Buffer;
      BufferLength := Stream.Size - 1;
      { Вызываем загрузчик }
      DeSerializeInternal(Component, Component.ClassName);
   FINALLY
      FreeMem(Buffer);
   END;
END;

{
  Рекурсивная процедура загрузки объекта их текстового буфера с XML
  Вызывается из:
    Serialize()
  Вход:
    Component - компонент для конвертации
    ComponentTagName - имя XML тега объекта
    ParentBlockEnd - указатель на конец XML описания родительского тега
}

PROCEDURE TJvgXMLSerializer.DeSerializeInternal(Component: TObject; {const}
   ComponentTagName: STRING; ParentBlockEnd: PChar = NIL);
VAR
   BlockStart, BlockEnd, TagStart, TagEnd: PChar;
   TagName, TagValue, TagValueEnd: PChar;
   TypeInf                    : PTypeInfo;
   TypeData                   : PTypeData;
   PropIndex                  : integer;
   AName                      : STRING;
   PropList                   : PPropList;
   NumProps                   : word;

   { Поиск у объекта свойства с заданным именем }

   FUNCTION FindProperty(TagName: PChar): integer;
   VAR
      i                       : integer;
   BEGIN
      Result := -1;
      FOR i := 0 TO NumProps - 1 DO
         IF CompareStr(PropList^[i]^.Name, TagName) = 0 THEN
         BEGIN
            Result := i;
            break;
         END;
   END;

   PROCEDURE SkipSpaces(VAR TagEnd: PChar);
   BEGIN
      WHILE TagEnd[0] <= #33 DO
         inc(TagEnd);
   END;

   {
     StrPosExt - ищет позицию одной строки в другой с заданной длиной.
     На длинных строках превосходит StrPos.
   }

   FUNCTION StrPosExt(CONST Str1, Str2: PChar; Str2Len: DWORD): PChar;
      ASSEMBLER;
   ASM
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
   END;

BEGIN
   { Playing with RTTI }
   TypeInf := Component.ClassInfo;
   AName := TypeInf^.Name;
   TypeData := GetTypeData(TypeInf);
   NumProps := TypeData^.PropCount;

   GetMem(PropList, NumProps * sizeof(pointer));

   IF NOT WrapCollections AND (Component IS TCollection) THEN
      ComponentTagName := TCollection(Component).ItemClass.ClassName;

   TRY
      GetPropInfos(TypeInf, PropList);

      { ищем открывающий тег }
      BlockStart := StrPosExt(TokenPtr, PChar('<' + ComponentTagName + '>'),
         BufferLength);

      { Если тег не найден и его наличие необязательно, то не обрабатываем его }
      IF (BlockStart = NIL) AND NOT StrongConformity THEN
         exit;

      { иначе проверяем его присутствие }
      check(BlockStart <> NIL, 'Открывающий тег не найден: ' + '<' +
         ComponentTagName + '>');
      inc(BlockStart, length(ComponentTagName) + 2);

      { ищем закрывающий тег }
      BlockEnd := StrPosExt(BlockStart, PChar('</' + ComponentTagName + '>'),
         BufferLength);
      check(BlockEnd <> NIL, 'Закрывающий тег не найден: ' + '<' +
         ComponentTagName + '>');

      { проверка на вхождение закр. тега в родительский тег }
      check((ParentBlockEnd = NIL) OR (BlockEnd < ParentBlockEnd),
         'Закрывающий тег не найден: ' + '<' + ComponentTagName + '>');

      TagEnd := BlockStart;
      SkipSpaces(TagEnd);

      { XML парсер }
      WHILE (TagEnd < BlockEnd) { and (TagEnd >= TokenPtr)} DO
      BEGIN
         { быстрый поиск угловых скобок }
         ASM
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
         END;

         GetMem(TagName, TagEnd - TagStart + 1);
         TRY

            { TagName - имя тега }
            StrLCopy(TagName, TagStart + 1, TagEnd - TagStart - 1);

            { TagEnd - закрывающий тег }
            TagEnd := StrPosExt(TagEnd, PChar('</' + TagName + '>'),
               BufferLength);

            //inc(TagStart, length('</' + TagName + '>')-1);

            { начало очередного дочернего тега }
            TagValue := TagStart + length('</' + TagName + '>') - 1;
            TagValueEnd := TagEnd;

            { поиск свойства, соответствующего тегу }
            PropIndex := FindProperty(TagName);

            IF NOT WrapCollections AND (PropIndex = -1) THEN
            BEGIN
               PropIndex := FindProperty(Pchar(TagName + 's'));

            END
            ELSE
               TokenPtr := TagStart;

            IF NOT IgnoreUnknownTags THEN
               check(PropIndex <> -1,
                  'TJvgXMLSerializer.DeSerializeInternal: Uncknown property: ' +
                  TagName);

            IF PropIndex <> -1 THEN
               SetPropertyValue(Component, PropList^[PropIndex], TagValue,
                  TagValueEnd, BlockEnd);

            inc(TagEnd, length('</' + TagName + '>'));
            SkipSpaces(TagEnd);

         FINALLY
            FreeMem(TagName);
         END;

      END;

   FINALLY
      FreeMem(PropList, NumProps * sizeof(pointer));
   END;

END;

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

PROCEDURE TJvgXMLSerializer.SetPropertyValue(Component: TObject; PropInfo:
   PPropInfo; Value, ValueEnd: PChar; ParentBlockEnd: PChar);
VAR
   PropTypeInf                : PTypeInfo;
   PropObject                 : TObject;
   CollectionItem             : TCollectionItem;
   sValue                     : STRING;
   charTmp                    : char;
BEGIN
   PropTypeInf := PropInfo.PropType^;

   CASE PropTypeInf^.Kind OF
      tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet,
         tkWChar, tkLString, tkWString, tkVariant:
         BEGIN
            { имитируем zero terminated string }
            charTmp := ValueEnd[0];
            ValueEnd[0] := #0;
            sValue := StrPas(Value);
            ValueEnd[0] := charTmp;

            { Замена спецсимволов. Актуально только для XML,
             сохраненного с помощью этого компонента }
            IF FReplaceReservedSymbols THEN
            BEGIN
               sValue := StringReplace(sValue, '&lt;', '<', [rfReplaceAll]);
               sValue := StringReplace(sValue, '&gt;', '>', [rfReplaceAll]);
               // sValue := StringReplace(sValue, '&', '&', [rfReplaceAll]);
            END;

            { Замена разделителя на системный }
            IF PropTypeInf^.Kind = tkFloat THEN
            BEGIN
               IF DecimalSeparator = ',' THEN
                  sValue := StringReplace(sValue, '.', DecimalSeparator,
                     [rfReplaceAll])
               ELSE
                  sValue := StringReplace(sValue, ',', DecimalSeparator,
                     [rfReplaceAll]);
            END;

            { Для корректного преобразования парсером tkSet нужны угловые скобки }
            IF PropTypeInf^.Kind = tkSet THEN
               sValue := '[' + sValue + ']';
            SetPropValue(Component, PropInfo^.Name, sValue);
         END;
      tkClass:
         BEGIN
            PropObject := GetObjectProp(Component, PropInfo);
            IF Assigned(PropObject) THEN
            BEGIN
               { Индивидуальный подход к некоторым классам }
               IF (PropObject IS TStrings) THEN { Текстовые списки }
               BEGIN
                  charTmp := ValueEnd[0];
                  ValueEnd[0] := #0;
                  sValue := StrPas(Value);
                  ValueEnd[0] := charTmp;
                  TStrings(PropObject).CommaText := sValue;
               END
               ELSE IF (PropObject IS TCollection) THEN { Коллекции }
               BEGIN
                  WHILE true DO { Заранее не известно число элементов в коллекции }
                  BEGIN
                     CollectionItem := (PropObject AS TCollection).Add;
                     TRY
                        DeSerializeInternal(CollectionItem,
                           CollectionItem.ClassName, ParentBlockEnd);
                     EXCEPT { Исключение, если очередной элемент не найден }
                        ON E: Exception DO
                        BEGIN
                           // Application.MessageBox(PChar(E.Message), '', MB_OK); - debug string
                           CollectionItem.Free;
                           // raise;  - debug string
                           break;
                        END;
                     END;
                  END;
               END
               ELSE { Для остальных классов - рекурсивная обработка }
                  DeSerializeInternal(PropObject, PropInfo^.Name,
                     ParentBlockEnd);
            END;

         END;
   END;
END;

{
  Процедура генерации DTD для заданного объекта в
  соответствии с published интерфейсом его класса.
  Вход:
    Component - объект
  Выход:
    текст DTD в поток Stream
}

PROCEDURE TJvgXMLSerializer.GenerateDTD(Component: TObject; Stream: TStream);
VAR
   DTDList                    : TStringList;
BEGIN
   DTDList := TStringList.Create;
   TRY
      GenerateDTDInternal(Component, DTDList, Stream, Component.ClassName);
   FINALLY
      DTDList.Free;
   END;
END;

{
  Внутренняя рекурсивная процедура генерации DTD для заданного объекта.
  Вход:
    Component - объект
    DTDList - список уже определенных элементов DTD
              для предотвращения повторений.
  Выход:
    текст DTD в поток Stream
}

PROCEDURE TJvgXMLSerializer.GenerateDTDInternal(Component: TObject; DTDList:
   TStrings; Stream: TStream; CONST ComponentTagName: STRING);
VAR
   PropInfo                   : PPropInfo;
   TypeInf, PropTypeInf       : PTypeInfo;
   TypeData                   : PTypeData;
   i                          : integer;
   AName, PropName, TagContent: STRING;
   PropList                   : PPropList;
   NumProps                   : word;
   PropObject                 : TObject;
CONST
   PCDATA                     = '#PCDATA';

   PROCEDURE addElement(CONST ElementName: STRING; Data: STRING);
   VAR
      s                       : STRING;
   BEGIN
      IF DTDList.IndexOf(ElementName) <> -1 THEN
         exit;
      DTDList.Add(ElementName);
      s := '<!ELEMENT ' + ElementName + ' ';
      IF Data = '' THEN
         Data := PCDATA;
      s := s + '(' + Data + ')>'#13#10;
      Stream.Write(PChar(s)[0], length(s));
   END;
BEGIN
   { Playing with RTTI }
   TypeInf := Component.ClassInfo;
   AName := TypeInf^.Name;
   TypeData := GetTypeData(TypeInf);
   NumProps := TypeData^.PropCount;

   GetMem(PropList, NumProps * sizeof(pointer));
   TRY
      { Получаем список свойств }
      GetPropInfos(TypeInf, PropList);
      TagContent := '';

      FOR i := 0 TO NumProps - 1 DO
      BEGIN
         PropName := PropList^[i]^.Name;

         PropTypeInf := PropList^[i]^.PropType^;
         PropInfo := PropList^[i];

         { Пропустить не поддерживаемые типы }
         IF NOT (PropTypeInf^.Kind IN [tkDynArray, tkArray, tkRecord,
            tkInterface, tkMethod]) THEN
         BEGIN
            IF TagContent <> '' THEN
               TagContent := TagContent + '|';
            TagContent := TagContent + PropName;
         END;

         CASE PropTypeInf^.Kind OF
            tkInteger, tkChar, tkFloat, tkString,
               tkWChar, tkLString, tkWString, tkVariant, tkEnumeration, tkSet:
               BEGIN
                  { Перевод в DTD. Для данных типов модель содержания - #PCDATA }
                  addElement(PropName, PCDATA);
               END;
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
               BEGIN
                  PropObject := GetObjectProp(Component, PropInfo);
                  IF Assigned(PropObject) THEN
                  BEGIN
                     { Для дочерних свойств-классов - рекурсивный вызов }
                     IF (PropObject IS TPersistent) THEN
                        GenerateDTDInternal(PropObject, DTDList, Stream,
                           PropName);
                  END;
               END;

         END;
      END;

      { Индивидуальный подход к некоторым классам }
      { Для коллекций необходимо включить в модель содержания тип элемента }
      IF (Component IS TCollection) THEN
      BEGIN
         IF TagContent <> '' THEN
            TagContent := TagContent + '|';
         TagContent := TagContent + (Component AS
            TCollection).ItemClass.ClassName + '*';
      END;

      { Добавляем модель содержания для элемента }
      addElement(ComponentTagName, TagContent);
   FINALLY
      FreeMem(PropList, NumProps * sizeof(pointer));
   END;

END;

PROCEDURE TJvgXMLSerializer.check(Expr: boolean; CONST Message: STRING);
BEGIN
   IF NOT Expr THEN
      RAISE
         EJvgXMLSerializerException.Create('EJvgXMLSerializerException'#13#10#13#10
         + Message);
END;

END.

//(PShortString(@(GetTypeData(GetTypeData(PropTypeInf)^.BaseType^).NameList)))

//tickCount := GetTickCount();
//inc(tickCounter, GetTickCount() - tickCount);

