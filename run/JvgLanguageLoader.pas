{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLanguageLoader.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Burov Dmitry, translation of russian text.

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{
eng:
 Load new string resources from file to components. Uses RTTI

 Replaces the found lines(strings) from one language to another if found in dictionary

 Dictionary is text like:
   Langauge1_line=Language2_line
   ..
   Language1_line=Language2_line
}

// rus:
// Заменяет по словарю из файла все найденные строки одного языка на другой
//  Словарь в виде текста вида:
//  Строка на языке 1=Строка на языке 2
//  ...
//  Строка на языке 1=Строка на языке 2
// ===================================================================
//

unit JvgLanguageLoader;

interface

uses
  Windows, Messages, SysUtils, Classes, JVComponent, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Grids;

type
  TLanguageLoaderOptions = set of (lofTrimSpaces);
  //{опция удаления начальных и завершающих пробелов}
  { Option to Trim first and last spaces [translated] }

  TJvgLanguageLoader = class(TJvComponent)
  private
    sl: TStringList;
    FOptions: TLanguageLoaderOptions;
    function TranslateString(sString: string): string;
  protected
    procedure UpdateComponent(Component: TPersistent); virtual;
  public
    procedure LoadLanguage(Component: TComponent; FileName: string); {main function}
  published
    property Options: TLanguageLoaderOptions read FOptions write FOptions;
  end;

procedure LoadLanguage(Component: TComponent; FileName: string; Options:
  TLanguageLoaderOptions);

implementation

uses
  TypInfo;

//{Ф-ия для загрузки словаря без предварительного создания компонента}
{ Function to load dictionary without previous creation of the component [translated] }

procedure LoadLanguage(Component: TComponent; FileName: string; Options:
  TLanguageLoaderOptions);
var
  LanguageLoader: TJvgLanguageLoader;
begin
  LanguageLoader := TJvgLanguageLoader.Create(nil);
  try
    LanguageLoader.LoadLanguage(Component, FileName);
  finally
    LanguageLoader.Free;
  end;
end;

{ TJvgLanguageLoader }

//{  Загрузка словаря, обход указанного компонента и  }
//{  всех его дочерних компонентов                    }
{ Loading dictionary, passing the given component and all his children components
  [translated] }

procedure TJvgLanguageLoader.LoadLanguage(Component: TComponent; FileName:
  string);

  procedure UpdateAllComponents(Component: TComponent);
  var
    i: integer;
  begin
    //{ обработка своцств компонента }
    { Processing the component's properties [translated] }
    UpdateComponent(Component);
    for i := 0 to Component.ComponentCount - 1 do
      UpdateAllComponents(Component.Components[i]);
  end;
begin
  sl := TStringList.Create;
  try
    //{ Загрузка словаря из заданного файла }
    { Loading dictionary from given file }
    sl.LoadFromFile(FileName);
    sl.Sorted := true;
    UpdateAllComponents(Component);
  finally
    sl.Free;
  end;
end;

//{ Проход по всем свойствам компонента                        }
//{ Для всех строковых свойств - загрузка перевода из сооваря  }
{ Passing(iterating) all the component's properties
  For all string properties - load the translation from dictionary [translated] }

procedure TJvgLanguageLoader.UpdateComponent(Component: TPersistent);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  i, j: integer;
  AName, PropName, StringPropValue: string;
  PropList: PPropList;
  NumProps: word;
  PropObject: TObject;
begin
  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := TypeInf^.Name;
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps * sizeof(pointer));

  try
    GetPropInfos(TypeInf, PropList);

    for i := 0 to NumProps - 1 do
    begin
      PropName := PropList^[i]^.Name;

      PropTypeInf := PropList^[i]^.PropType^;
      PropInfo := PropList^[i];

      case PropTypeInf^.Kind of
        tkString, tkLString:
          //{ Переводить свойство Name не следует }
          { .Name is not to be translated [translated] }
          if PropName <> 'Name' then
          begin
            //{ Получение значения свойства и поиск перевода в словаре }
            { Retrieving the property's value and searchin for translation in
              dictionary [translated] }
            StringPropValue := GetStrProp(Component, PropInfo);
            SetStrProp(Component, PropInfo,
              TranslateString(StringPropValue));
          end;
        tkClass:
          begin
            PropObject := GetObjectProp(Component, PropInfo
              {, TPersistent});

            if Assigned(PropObject) then
            begin
              //{ Для дочерних свойств-классов вызов просмотра свойств }
              { For children properties-classes calling iterate again [translated] }
              if (PropObject is TPersistent) then
                UpdateComponent(PropObject as TPersistent);

              //{ Индивидуальный подход к некоторым классам }
              { Specific handling of some certain classes [translated] }
              if (PropObject is TStrings) then
              begin
                for j := 0 to (PropObject as TStrings).Count - 1 do
                  TStrings(PropObject)[j] :=
                    TranslateString(TStrings(PropObject)[j]);
              end;
              if (PropObject is TTreeNodes) then
              begin
                for j := 0 to (PropObject as TTreeNodes).Count - 1 do
                  TTreeNodes(PropObject).Item[j].Text :=
                    TranslateString(TTreeNodes(PropObject).Item[j].Text);
              end;
              if (PropObject is TListItems) then
              begin
                for j := 0 to (PropObject as TListItems).Count - 1 do
                  TListItems(PropObject).Item[j].Caption :=
                    TranslateString(TListItems(PropObject).Item[j].Caption);
              end;
              //{ Здесь можно добавить обработку остальных классов }
              { And here may be added more specific handlers for certain other
                classes [translated] }
            end;
          end;
      end;
    end;
  finally
    FreeMem(PropList, NumProps * sizeof(pointer));
  end;
end;

//{ Поиск перевода для заданной строки в словаре }
{ Searching for translation of given line in dictionary [translated] }

function TJvgLanguageLoader.TranslateString(sString: string): string;
begin
  if lofTrimSpaces in Options then
    sString := trim(sString);
  if sString = '' then
  begin
    Result := '';
    exit;
  end;
  if sl.IndexOfName(sString) <> -1 then
    Result := sl.Values[sString]
  else
    Result := sString;
end;

end.

