{ 
  								  
 		 Globus Delphi VCL Extensions Library		   
 			  ' GLOBUS LIB '			   
  			     Freeware				  
  	  Copyright (c) 1998,2001 Chudin A.V, chudin@yandex.ru	  
  								  
  
 ===================================================================
 glLanguageLoader Unit 04.2001	         component TglLanguageLoader
 ===================================================================
eng:
 Load new string resources from file to components. Uses RTTI

rus:
Заменяет по словарю из файла все найденные строки одного языка на другой
 Словарь в виде текста вида:
 Строка на языке 1=Строка на языке 2
 ...
 Строка на языке 1=Строка на языке 2
 ===================================================================
}
unit glLanguageLoader;

interface
{$I glDEF.INC}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, comctrls, grids;

type
  TLanguageLoaderOptions = set of (lofTrimSpaces);
  {опция удаления начальных и завершающих пробелов}

  TglLanguageLoader = class(TComponent)
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

procedure LoadLanguage(Component: TComponent; FileName: string; Options: TLanguageLoaderOptions);

procedure Register;

implementation
uses TypInfo ;//{$IFDEF GLVER_D6}, DesignIntf, DesignWindows, DesignEditors{$ELSE} {$IFDEF GLVER_D5}, dsgnintf{$ENDIF} {$ENDIF};

procedure Register;
begin
  RegisterComponents('Gl Components', [TglLanguageLoader]);
end;

{Ф-ия для загрузки словаря без предварительного создания компонента}
procedure LoadLanguage(Component: TComponent; FileName: string; Options: TLanguageLoaderOptions);
var
  LanguageLoader: TglLanguageLoader;
begin
  LanguageLoader := TglLanguageLoader.Create(nil);
  try
      LanguageLoader.LoadLanguage(Component, FileName);
  finally
    LanguageLoader.Free;
  end;
end;

{ TglLanguageLoader }

{  Загрузка словаря, обход указанного компонента и  }
{  всех его дочерних компонентов                    }
procedure TglLanguageLoader.LoadLanguage(Component: TComponent; FileName: string);
  procedure UpdateAllComponents(Component: TComponent);
  var i: integer;
  begin
    { обработка своцств компонента }
    UpdateComponent(Component);
    for i := 0 to Component.ComponentCount-1 do
      UpdateAllComponents(Component.Components[i]);
  end;
begin
  sl := TStringList.Create;
  try
    { Загрузка словаря из заданного файла }
    sl.LoadFromFile(FileName);
    sl.Sorted := true;
    UpdateAllComponents(Component);
  finally
    sl.Free;
  end;
end;

{ Проход по всем свойствам компонента                        }
{ Для всех строковых свойств - загрузка перевода из сооваря  }
procedure TglLanguageLoader.UpdateComponent(Component: TPersistent);
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

  GetMem(PropList, NumProps*sizeof(pointer));

  try
    GetPropInfos(TypeInf, PropList);

    for i := 0 to NumProps-1 do
    begin
      PropName := PropList^[i]^.Name;

      PropTypeInf := PropList^[i]^.PropType^;
      PropInfo := PropList^[i];


      case PropTypeInf^.Kind of
        tkString, tkLString:
        if PropName <> 'Name' then { Переводить свойство Name не следует }
        begin
          { Получение значения свойства и поиск перевода в словаре }
          StringPropValue := GetStrProp( Component, PropInfo );
          SetStrProp( Component, PropInfo, TranslateString(StringPropValue) );
        end;
        tkClass:
        begin
          PropObject := GetObjectProp(Component, PropInfo{, TPersistent});

          if Assigned(PropObject)then
          begin
            { Для дочерних свойств-классов вызов просмотра свойств }
            if (PropObject is TPersistent) then
             UpdateComponent(PropObject as TPersistent);

            { Индивидуальный подход к некоторым классам }
            if (PropObject is TStrings) then
            begin
              for j := 0 to (PropObject as TStrings).Count-1 do
                TStrings(PropObject)[j] := TranslateString(TStrings(PropObject)[j]);
            end;
            if (PropObject is TTreeNodes) then
            begin
              for j := 0 to (PropObject as TTreeNodes).Count-1 do
                TTreeNodes(PropObject).Item[j].Text := TranslateString(TTreeNodes(PropObject).Item[j].Text);
            end;
            if (PropObject is TListItems) then
            begin
              for j := 0 to (PropObject as TListItems).Count-1 do
                TListItems(PropObject).Item[j].Caption := TranslateString(TListItems(PropObject).Item[j].Caption);
            end;
            { Здесь можно добавить обработку остальных классов }
          end;

        end;

      end;
    end;
  finally
    FreeMem(PropList, NumProps*sizeof(pointer));
  end;
end;

{ Поиск перевода для заданной строки в словаре }
function TglLanguageLoader.TranslateString(sString: string): string;
begin
  if lofTrimSpaces in Options then sString := trim(sString);
  if sString = '' then
  begin
    Result := '';
    exit;
  end;
  if sl.IndexOfName(sString) <> -1 then Result := sl.Values[sString] else Result := sString;
end;


end.
