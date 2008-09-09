{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLanguageLoader.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
eng:
  Load new string resources from file to components. Uses RTTI

  Replaces the found lines(strings) from one language to another if found in dictionary

  Dictionary is text like:
    Langauge1_line=Language2_line
    ..
    Language1_line=Language2_line

rus:
  Заменяет по словарю из файла все найденные строки одного языка на другой
  Словарь в виде текста вида:
  Строка на языке 1=Строка на языке 2
  ...
  Строка на языке 1=Строка на языке 2

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgLanguageLoader;

{$I jvcl.inc}

interface

uses
  {$IFDEF USEJVCL}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF USEJVCL}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF USEJVCL}
  ComCtrls,
  JvComponentBase;
  {$ELSE}
  ComCtrls;
  {$ENDIF USEJVCL}

type
  TLanguageLoaderOptions = set of (lofTrimSpaces, lofConvertEscapes);
  // lofConvertEscapes: if set (=default) converts escape sequences when reading
  // language strings form language file, eg \n = #10 (Note: only used in
  // FTranslationStrings, not in FTranslations
  //{опция удаления начальных и завершающих пробелов}
  { Option to Trim first and last spaces [translated] }

  {$IFDEF USEJVCL}
  TJvgLanguageLoader = class(TJvComponent)
  {$ELSE}
  TJvgLanguageLoader = class(TComponent)
  {$ENDIF USEJVCL}
  private
    // FTranslations: temporary stringlist with translations of strings on a form
    // Note: is destroyed after the translation is complete!
    FTranslations: TStringList;

    // FOldStrings: temporary stringlist needed when changing translations
    // Note: is destroyed after the translation is complete!
    FOldStrings: TStringList;

    // FTranslationChange: used internally. Set to true when changing translations
    // This way we now we have to look up the original text in FOldStrings.
    FTranslationChange: Boolean;

    // FOptions: some translation options
    FOptions: TLanguageLoaderOptions;

    // FSaveEmpty doesn't work at the moment? (include in options?)
    //FSaveEmpty: Boolean; //JGB: save empty strings or not (default=false)

    // FDictionaryFileName: Name and path of the language dictionary file
    FDictionaryFileName: string;

    // FFormSection: section in languagefile where the translation of the
    // current form can be found
    // (empty=default=[<FormName>] (if Owner=nil: [Translation])
    FFormSection: string;

    // FStringsSection: section in languagefile where the translation of
    // string consts can be found
    // (empty=default=[<FormName>.Strings] (if Owner=nil: [Translation.Strings])
    FStringsSection: string;

    // FTranslationStrings: list of string translations!
    // Read form FStringSection of language file
    FTranslationStrings: TStringList;

    // FIgnoreList: List of component names that should not be translated e.g. contents of a RichEdit control
    FIgnoreList: TStringList;

    // DEPRECATED
    function TranslateString(AText: string): string;

    procedure SetIgnoreList(const Value: TStringList);
    procedure SetTranslationStrings(const Value: TStringList);
    function GetFormSection: string;
    function GetStringsSection: string;
  protected
    procedure UpdateComponent(Component: TPersistent); virtual;
    procedure GetAllComponentStrings(Component: TPersistent); virtual;

    // Changed to internal function
    procedure LoadLanguage(Component: TComponent; const FileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // TranslateComponent: Translate the said component using the language file
    // specified in FDictionaryFileName
    procedure TranslateComponent(Component: TComponent; DoLoadStrings: Boolean);

    // ChangeTranslation: As TranslateComponent but now change an already
    // translated component to another language. DictionaryFileName should
    // contain the new language filename and the old (current) language file
    // should be in OldLanguageFilename
    procedure ChangeTranslation(Component: TComponent; DoLoadStrings: Boolean;
      OldLanguageFileName: string);

    // LoadStrings: Loads strings form the FStringsSection of the languagefile
    // into FTranslationStrings, after that it can be used in MessageBox and Translate
    procedure LoadStrings;

    // JGB - Save all strings on a form (equivalent to LoadLanguage
    procedure SaveAllStrings(Component: TComponent; FileName: string);

    // JGB - Translation wrapper for Application.Messagebox
    // Call this instead of Application.Messagebox when translation is needed
    function MessageBox(const Text, Caption: PChar; Flags: Longint = MB_OK): Integer;
    // Maybe more like MessageBox needed?

    // JGB - Translate: use this to translate a string loaded in ... stringlist (todo)
    function Translate(const Text: string): string;

    property TranslationStrings: TStringList read FTranslationStrings
      write SetTranslationStrings;
  published
    property Options: TLanguageLoaderOptions read FOptions write FOptions
      default [lofTrimSpaces, lofConvertEscapes];
    property DictionaryFileName: string read FDictionaryFileName
      write FDictionaryFileName;
    property FormSection: string read GetFormSection write FFormSection;
    property StringsSection: string read GetStringsSection write FStringsSection;
    property IgnoreList: TStringList read FIgnoreList write SetIgnoreList;
  end;


procedure LoadLanguage(Component: TComponent; FileName: string; Options:
  TLanguageLoaderOptions);

{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

implementation

{.$DEFINE DEBUG_LANG}
uses {$IFDEF DEBUG_LANG}DbugIntf,{$ENDIF}
  TypInfo, IniFiles, JclStrings;

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

constructor TJvgLanguageLoader.Create(AOwner: TComponent);
begin
  inherited;
  Options := [lofTrimSpaces,lofConvertEscapes];
  FIgnoreList := TStringList.Create;
  FIgnoreList.Sorted := True;
{$IFDEF COMPILER6_UP}
  FIgnoreList.CaseSensitive := True;
{$ENDIF COMPILER6_UP}

  FTranslationStrings := TStringList.Create;
  FTranslationStrings.Sorted := True;
{$IFDEF COMPILER6_UP}
  FTranslationStrings.CaseSensitive := True;
{$ENDIF COMPILER6_UP}
end;

destructor TJvgLanguageLoader.Destroy;
begin
  FreeAndNil(FIgnoreList);
  FreeAndNil(FTranslationStrings);
  inherited Destroy;
end;

procedure TJvgLanguageLoader.SetIgnoreList(const Value: TStringList);
begin
  FIgnoreList.Assign(Value);
end;

procedure TJvgLanguageLoader.SetTranslationStrings(const Value: TStringList);
begin
  FTranslationStrings.Assign(Value);
end;

function TJvgLanguageLoader.GetFormSection: string;
begin
  if FFormSection <> '' then
    Result := FFormSection
  else
  if Owner <> nil then
    Result := Owner.Name
  else
    Result := 'Translation';
end;

function TJvgLanguageLoader.GetStringsSection: string;
begin
  if FStringsSection <> '' then
    Result := FStringsSection
  else
  if Owner <> nil then
    Result := Owner.Name
  else
    Result := 'Translation.Strings';
end;

//{  Загрузка словаря, обход указанного компонента и  }
//{  всех его дочерних компонентов                    }
{ Loading dictionary, passing the given component and all his children components
  [translated] }

procedure TJvgLanguageLoader.LoadLanguage(Component: TComponent; const FileName: string);
var
  IniFile: TCustomIniFile;

  procedure UpdateAllComponents(Component: TComponent);
  var
    I: Integer;
  begin
    //{ обработка своцств компонента }
    { Processing the component's properties [translated] }
    UpdateComponent(Component);
    for I := 0 to Component.ComponentCount - 1 do
      UpdateAllComponents(Component.Components[I]);
  end;

begin
  FTranslations := TStringList.Create;
  IniFile := TIniFile.Create(FileName);
  try
    //{ Загрузка словаря из заданного файла }
    { Loading dictionary from given file }
    FTranslations.Sorted := true;
{$IFDEF COMPILER6_UP}
    FTranslations.CaseSensitive := True;
{$ENDIF COMPILER6_UP}
    IniFile.ReadSectionValues(FormSection,FTranslations);
    UpdateAllComponents(Component);
  finally
    FTranslations.Free;
    FreeAndNil(IniFile);
  end;
end;

// TranslateComponent: Translate the said component using the language file
// specified in FDictionaryFileName
// if DoLoadStrings = true then LoadStrings is called after translating the form
procedure TJvgLanguageLoader.TranslateComponent(Component: TComponent;
  DoLoadStrings: Boolean);
begin
  if DictionaryFileName <> '' then
  begin
    LoadLanguage(Component,DictionaryFileName);
    if DoLoadStrings then
      LoadStrings;
  end;
end;

// ChangeTranslation: As TranslateComponent but now change an already
// translated component to another language. DictionaryFileName should
// contain the new language filename and the old (current) language file
// should be in OldLanguageFilename
procedure TJvgLanguageLoader.ChangeTranslation(Component: TComponent;
  DoLoadStrings: Boolean; OldLanguageFileName: string);
var
  OldIniFile: TCustomIniFile;
  I: Integer;
  EqualPos, Len: Integer;
  TempStr: string;
begin
  if (DictionaryFileName <> '') and (OldLanguageFileName <> '') then
  begin
    FOldStrings := TStringList.Create;
    OldIniFile := TIniFile.Create(OldLanguageFileName);
    try
{$IFDEF COMPILER6_UP}
      FOldStrings.CaseSensitive := True;
{$ENDIF COMPILER6_UP}
      FOldStrings.Sorted := False;
      OldIniFile.ReadSectionValues(FormSection, FOldStrings);
      FOldStrings.BeginUpdate;
      for I := 0 to FOldStrings.Count-1 do
      begin
        TempStr := FOldStrings.Strings[I];
        EqualPos := Pos('=', TempStr);
        if EqualPos > 0 then // Found: exchange Name=Value
        begin
          Len := Length(TempStr);
          FOldStrings.Strings[I] := Copy(TempStr, EqualPos + 1, Len - EqualPos) + '=' +
            Copy(TempStr,1,EqualPos-1);
        end;
      end;
      FOldStrings.EndUpdate;
      FOldStrings.Sorted := True;
      FTranslationChange := True;
      LoadLanguage(Component, DictionaryFileName);
    finally
      FTranslationChange := False;
      FreeAndNil(FOldStrings);
      OldIniFile.Free;
    end;
    // *** Alas, Changing translation for the following is not possible!
    if DoLoadStrings then
      LoadStrings;
  end;
end;


// LoadStrings: Loads strings form the FStringsSection of the languagefile
// into FTranslationStrings, after that it can be used in MessageBox and Translate
procedure TJvgLanguageLoader.LoadStrings;
var
  I: Integer;
  IniFile: TCustomIniFile;
begin
  if DictionaryFileName <> '' then
  begin
    IniFile := TIniFile.Create(DictionaryFileName);
    try
      TranslationStrings.Clear; // Remove old translation strings
      TranslationStrings.Sorted := False; // no sort during processing
      IniFile.ReadSectionValues(StringsSection, TranslationStrings);
      if lofConvertEscapes in Options then
      begin
        TranslationStrings.BeginUpdate;
        for I := 0 to TranslationStrings.Count-1 do
          TranslationStrings.Strings[I] :=
            StrEscapedToString(TranslationStrings.Strings[i]);
        TranslationStrings.EndUpdate;
      end;
      TranslationStrings.Sorted := True; // and now sort once
    finally
      FreeAndNil(IniFile);
    end;
  end;
end;


// JGB - Save all strings on a form (equivalent to LoadLanguage
procedure TJvgLanguageLoader.SaveAllStrings(Component: TComponent; FileName: string);
var
  IniFile: TCustomIniFile;

  procedure GetAllComponents(Component: TComponent);
  var
    I: Integer;
  begin
    // Processing the component's properties [translated]
    GetAllComponentStrings(Component);
    for I := 0 to Component.ComponentCount - 1 do
      GetAllComponents(Component.Components[I]);
  end;

  procedure WriteSectionValues;
  var
    i: Integer;
  begin
    for I := 0 to FTranslations.Count-1 do
      IniFile.WriteString(FormSection, FTranslations.Names[i],FTranslations.Values[FTranslations.Names[i]]);
  end;

begin
  FTranslations := TStringList.Create;
  IniFile := TIniFile.Create(FileName);
  try
    // Saving dictionary to given file
    GetAllComponents(Component);
    WriteSectionValues; // write to dictionary
  finally
    FTranslations.Free;
    IniFile.Free;
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
  I, J, Idx: integer;
  AName, PropName, StringPropValue: string;
  PropList: PPropList;
  NumProps: word;
  PropObject: TObject;
  OldSort: Boolean;
  TempTrans: string;

  function NeedsTranslation(SourceStr: string; var DestStr: string): Boolean;
  begin
    DestStr := TranslateString(SourceStr);
    Result := SourceStr <> DestStr;
  end;

begin
  // JGB: First we look if this component is in the ignore list
  if IgnoreList.Find(Component.GetNamePath, Idx) then
    Exit; // Found in the list of excluded components so don't translate

  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(TypeInf^.Name);
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps * sizeof(Pointer));

  try
    GetPropInfos(TypeInf, PropList);

    for I := 0 to NumProps - 1 do
    begin
      PropName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropList^[I]^.Name);

      PropTypeInf := PropList^[I]^.PropType^;
      PropInfo := PropList^[I];

      case PropTypeInf^.Kind of
        {$IFDEF UNICODE} tkUString, {$ENDIF}
        tkString, tkLString:
          //{ Переводить свойство Name не следует }
          { .Name is not to be translated [translated] }
          if PropName <> 'Name' then
          begin
            //{ Получение значения свойства и поиск перевода в словаре }
            { Retrieving the property's value and searchin for translation in
              dictionary [translated] }
            StringPropValue := GetStrProp(Component, PropInfo);
            if NeedsTranslation(StringPropValue, TempTrans) then
              SetStrProp(Component, PropInfo, TempTrans);
          end;
        tkClass:
          begin
            PropObject := GetObjectProp(Component, PropInfo); {, TPersistent}

            if Assigned(PropObject) then
            begin
              //{ Для дочерних свойств-классов вызов просмотра свойств }
              { For children properties-classes calling iterate again [translated] }
              if PropObject is TPersistent then
                UpdateComponent(PropObject as TPersistent);

              //{ Индивидуальный подход к некоторым классам }
              { Specific handling of some certain classes [translated] }
              if PropObject is TStrings then
              begin
                TStrings(PropObject).BeginUpdate;
                try
                  if PropObject is TStringList then
                  begin
                    // It's problematic when a stringlist is sorted:
                    // so reset it for now
                    OldSort := (PropObject as TStringList).Sorted;
                    (PropObject as TStringList).Sorted := False;
                    for J := 0 to (PropObject as TStrings).Count - 1 do
                      if NeedsTranslation(TStrings(PropObject)[J], TempTrans) then
                        TStrings(PropObject)[J] := TempTrans;
                    (PropObject as TStringList).Sorted := OldSort;
                  end
                  else
                    for J := 0 to (PropObject as TStrings).Count - 1 do
                      if NeedsTranslation(TStrings(PropObject)[J], TempTrans) then
                        TStrings(PropObject)[J] := TempTrans;
                finally
                  TStrings(PropObject).EndUpdate;
                end;
              end;
              if PropObject is TTreeNodes then
              begin
                TTreeNodes(PropObject).BeginUpdate;
                try
                  for J := 0 to (PropObject as TTreeNodes).Count - 1 do
                    if NeedsTranslation(TTreeNodes(PropObject).Item[J].Text, TempTrans) then
                      TTreeNodes(PropObject).Item[J].Text := TempTrans;
                finally
                  TTreeNodes(PropObject).EndUpdate;
                end;
              end;
              if PropObject is TListItems then
              begin
                TListItems(PropObject).BeginUpdate;
                try
                  for J := 0 to (PropObject as TListItems).Count - 1 do
                    if NeedsTranslation(TListItems(PropObject).Item[J].Caption, TempTrans) then
                      TListItems(PropObject).Item[J].Caption := TempTrans;
                finally
                  TListItems(PropObject).EndUpdate;
                end;
              end;
              //{ Здесь можно добавить обработку остальных классов }
              { And here may be added more specific handlers for certain other
                classes [translated] }
            end;
          end;
      end;
    end;
  finally
    FreeMem(PropList, NumProps * sizeof(Pointer));
  end;
end;

{ Get all strings on this component that can be translated and put it in
  the dictionary. }

procedure TJvgLanguageLoader.GetAllComponentStrings(Component: TPersistent);
var
  PropInfo: PPropInfo;
  TypeInf, PropTypeInf: PTypeInfo;
  TypeData: PTypeData;
  I, J, Idx: integer;
  AName, PropName, StringPropValue: string;
  PropList: PPropList;
  NumProps: Word;
  PropObject: TObject;
begin
  // JGB: First we look if this component is in the ignore list
  if IgnoreList.Find(Component.GetNamePath, Idx) then
    Exit; // Found in the list of excluded components so don't translate

  { Playing with RTTI }
  TypeInf := Component.ClassInfo;
  AName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(TypeInf^.Name);
  TypeData := GetTypeData(TypeInf);
  NumProps := TypeData^.PropCount;

  GetMem(PropList, NumProps * sizeof(pointer));

  try
    GetPropInfos(TypeInf, PropList);

    for I := 0 to NumProps - 1 do
    begin
      PropName := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropList^[I]^.Name);

      PropTypeInf := PropList^[I]^.PropType^;
      PropInfo := PropList^[I];

      case PropTypeInf^.Kind of
        {$IFDEF UNICODE} tkUString, {$ENDIF}
        tkString, tkLString:
          { .Name is not to be translated [translated] }
          if PropName <> 'Name' then
          begin
            { Retrieving the property's value and storing in dictionary  }
            StringPropValue := GetStrProp(Component, PropInfo);
            if {FSaveEmpty or} (StringPropValue <> '') then
            begin
              //FTranslations.Add(PropName+'='+StringPropValue);
              FTranslations.Add(Component.GetNamePath + '.' + PropName);
              FTranslations.Values[Component.GetNamePath + '.' + PropName] := StringPropValue;
            end;
          end;
        tkClass:
          begin
            PropObject := GetObjectProp(Component, PropInfo); //{, TPersistent}

            if Assigned(PropObject) then
            begin
              { For children properties-classes calling iterate again [translated] }
              if (PropObject is TPersistent) then
                UpdateComponent(PropObject as TPersistent);

              { Specific handling of some certain classes [translated] }
              if (PropObject is TStrings) then
              begin
                for J := 0 to (PropObject as TStrings).Count - 1 do
                  if {FSaveEmpty or} (TStrings(PropObject)[J] <> '') then
                  begin
                    FTranslations.Add(Component.GetNamePath + '.' + PropName);
                    FTranslations.Values[Component.GetNamePath + '.' + PropName] :=
                      TStrings(PropObject)[J];
                  end;
              end;
              if (PropObject is TTreeNodes) then
              begin
                for J := 0 to (PropObject as TTreeNodes).Count - 1 do
                  if {FSaveEmpty or} (TTreeNodes(PropObject).Item[J].Text <> '') then
                  begin
                    FTranslations.Add(Component.GetNamePath + '.' + PropName);
                    FTranslations.Values[Component.GetNamePath + '.' + PropName] :=
                      TTreeNodes(PropObject).Item[J].Text;
                  end;
              end;
              if (PropObject is TListItems) then
              begin
                for J := 0 to (PropObject as TListItems).Count - 1 do
                  if {FSaveEmpty or} (TListItems(PropObject).Item[J].Caption <> '') then
                  begin
                    FTranslations.Add(Component.GetNamePath + '.' + PropName);
                    FTranslations.Values[Component.GetNamePath + '.' + PropName] :=
                      TListItems(PropObject).Item[J].Caption;
                  end;
              end;
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

function TJvgLanguageLoader.TranslateString(AText: string): string;
var
  TempStr: string;
begin
  if lofTrimSpaces in Options then
    AText := trim(AText);

  if FTranslationChange then
    TempStr := FOldStrings.Values[AText]
  else
    TempStr := AText;

  if FTranslations.IndexOfName(TempStr) <> -1 then
    Result := FTranslations.Values[TempStr]
  else
    Result := AText;
end;

// JGB - Translation wrapper for Application.Messagebox
// Call this instead of Application.Messagebox when translation is needed
function TJvgLanguageLoader.MessageBox(const Text, Caption: PChar; Flags: Longint = MB_OK): Integer;
begin
  Result := Application.MessageBox(PChar(Translate(string(Text))),PChar(Translate(string(Caption))),Flags);
end;

// JGB - Translate: use this to translate a string
function TJvgLanguageLoader.Translate(const Text: string): string;
begin
  // should be removed: spaces are always trimmed when using ini file
  if lofTrimSpaces in Options then
    Result := Trim(Text)
  else
    Result := Text;

  Result := FTranslationStrings.Values[Result];
  if Result = '' then
    Result := Text;
end;


{$IFDEF USEJVCL}
{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
{$ENDIF USEJVCL}

end.

