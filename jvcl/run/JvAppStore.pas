{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppStore.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jens Fudickar

Last Modified: 2003-11-13

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAppStore;

{
  General storage unit - provides with a basic storage backend component to store application
  specific data. Descendants can provide specific backends for registry, INI-files, DB, XML,
  etc. Should be used to provide a common interface for storing data as is done in some of
  the JVCL components (eg. JvFormPlacement/JvFormStorage).

  This was requested in one of the comments of the JVCL 3.0 Survey Results.

  Paths
  =====
  Paths are relative to the current path. Paths are specified using backslashes (\) between
  individual folders and the value. Paths starting with a backslash are always relative to the root
  storage (application specific root, absolute root path).

  Dots (.) are used to reference parent folders with the following rules:
  * a single dot (.) refers to the current folder
  * each additional dot moves up a level in the folder hierarchie, ie. "....\Here" refers to a
    folder three levels up from the current where a sub folder/value name "Here" is searched. Of
    course the normal (OS path) specification can be used as well ("..\..\..\Here" would be the
    same as the first example).

  Multiple backslashes without names between them are ignored ("Root\\Here" is the same as
  "Root\Here").
}

interface

uses
  Classes,
  TypInfo,
  JvComponent;

type
  TJvCustomAppStore = class;

  TAppStoreListItem = procedure(Sender: TJvCustomAppStore; const Path: string;
    const Index: Integer) of object;
  TAppStoreListDelete = procedure(Sender: TJvCustomAppStore; const Path: string;
    const First, Last: Integer) of object;
  TAppStorePropTranslate = procedure(Sender: TJvCustomAppStore; Instance: TPersistent;
    var Name: string; const Reading: Boolean) of object;

  TJvAppStoreOptions = class(TPersistent)
  private
    FBooleanAsString: Boolean;
    FBooleanStringTrueValues: string;
    FBooleanStringFalseValues: string;
    FEnumAsStr: Boolean;
    FSetAsStr: Boolean;
    FDateTimeAsString: Boolean;
    FFloatAsString: Boolean;
    FDefaultIfReadConvertError: Boolean;
    FDefaultIfValueNotExists: Boolean;
  protected
    procedure SetBooleanAsString(Value: Boolean); virtual;
    procedure SetBooleanStringTrueValues(Value: string); virtual;
    procedure SetBooleanStringFalseValues(Value: string); virtual;
    procedure SetEnumAsStr(Value: Boolean); virtual;
    procedure SetSetAsStr(Value: Boolean); virtual;
    procedure SetDateTimeAsStr(Value: Boolean); virtual;
    procedure SetFloatAsStr(Value: Boolean); virtual;
    procedure SetDefaultIfReadConvertError(Value: Boolean); virtual;
    procedure SetDefaultIfValueNotExists(Value: Boolean); virtual;
    function IsValueListString(Value, List: string): Boolean; virtual;
  public
    constructor Create;
    function DefaultTrueString: string;
    function DefaultFalseString: string;
    function IsValueTrueString(Value: string): Boolean;
    function IsValueFalseString(Value: string): Boolean;
  published
    property BooleanStringTrueValues: string read FBooleanStringTrueValues
      write SetBooleanStringTrueValues;
    property BooleanStringFalseValues: string read FBooleanStringFalseValues
      write SetBooleanStringFalseValues;
    property BooleanAsString: Boolean read FBooleanAsString write SetBooleanAsString default True;
    property EnumerationAsString: Boolean read FEnumAsStr write SetEnumAsStr default True;
    property SetAsString: Boolean read FSetAsStr write SetSetAsStr default False;
    property DateTimeAsString: Boolean read FDateTimeAsString write SetDateTimeAsStr default True;
    property FloatAsString: Boolean read FFloatAsString write SetFloatAsStr default False;
    property DefaultIfReadConvertError: Boolean read FDefaultIfReadConvertError
      write SetDefaultIfReadConvertError default False;
    property DefaultIfValueNotExists: Boolean read FDefaultIfValueNotExists
      write SetDefaultIfValueNotExists default True;
  end;

  TJvAppStoreOptionsClass = class of TJvAppStoreOptions;

  TAppStoreEnumOption  = (
    aeoFolders,           // report folders
    aeoValues,            // report values
    aeoReportListAsValue, // report list as value (a list is actually a folder containing a Count and Item? values)
    aeoReportRelative,    // report all found folders and values relative to the requested path (otherwise relative to the Root path)
    aeoRecursive);        // scan sub folders as well
  TAppStoreEnumOptions = set of TAppStoreEnumOption;

  TJvCustomAppStore = class(TJvComponent)
  private
    FRoot: string;
    FCurPath: string;
    FStoreSL: TStrings;
    FStoreOptions: TJvAppStoreOptions;
  protected
    //Returns the property count of an instance
    function GetPropCount(Instance: TPersistent): Integer;
    //Returns the property name of an instance at a certain index
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    { Retrive the class that holds the storage options and format settings. }
    class function GetStoreOptionsClass: TJvAppStoreOptionsClass; virtual;
    { Split the specified path into an absolute path and a value name (the last item in the path
      string). Just a helper for all the storage methods. }
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); virtual;
    { Retrieve application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    function GetRoot: string; virtual;
    { Set application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    procedure SetRoot(Value: string); virtual;
    { Retrieves currently set path (including the Root path). }
    function GetCurrentPath: string;
    { Returns the path as an absolute path (including the Root path). If the given path does not
      start with a backslash (\) the path is appended to the Root path, resolving any references to
      parent folders. }
    function GetAbsPath(Path: string): string;
    { StringList item reader used by ReadStringList in the call to ReadList. }
    procedure ReadSLItem(Sender: TJvCustomAppStore; const Path: string;
      const Index: Integer);
    { StringList item writer used by WriteStringList in the call to WriteList. }
    procedure WriteSLItem(Sender: TJvCustomAppStore; const Path: string;
      const Index: Integer);
    { StringList item deleter used by WriteStringList in the call to WriteList. }
    procedure DeleteSLItems(Sender: TJvCustomAppStore; const Path: string;
      const First, Last: Integer);
    { Enum all folders in the specified folder. }
    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); virtual; abstract;
    { Enum all values below in the specified folder. }
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); virtual; abstract;
    { Internal retrieval of GetStoredValues. Is used to handle recursiveness. }
    procedure InternalGetStoredValues(const PrefixPath, SearchPath: string;
      const Strings: TStrings; const Options: TAppStoreEnumOptions);
    { Current root path for storage. Paths used in other methods are relative to this path. }
    function GetPath: string; virtual;
    { Specify a new root. Given path is relative to the current path. Se remarks above }
    procedure SetPath(const Path: string); virtual;
    { Determines if the specified name belongs to a list value. }
    class function NameIsListItem(Name: string): boolean;
    { Application specific root. Path is prepended to any specified path and serves as an absolute
      root for any reading/writing. Not all implementation will use it. Generally it's used for
      storages not specific to an application (such as the registry). }
    property Root: string Read GetRoot Write SetRoot;
    { Set the StoreOptions Property }
    procedure SetStoreOptions(Value: TJvAppStoreOptions);

    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function ReadIntegerInt(const Path: string; Default: Integer = 0): Integer; virtual; abstract;
    { Stores an Integer value. }
    procedure WriteIntegerInt(const Path: string; Value: Integer); virtual; abstract;
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised.}
    function ReadFloatInt(const Path: string; Default: Extended = 0): Extended; virtual; abstract;
    { Stores an Extended value. }
    procedure WriteFloatInt(const Path: string; Value: Extended); virtual; abstract;
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function ReadStringInt(const Path: string; Default: string = ''): string; virtual; abstract;
    { Stores an string value. }
    procedure WriteStringInt(const Path: string; Value: string); virtual; abstract;
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function ReadDateTimeInt(const Path: string; Default: TDateTime = 0): TDateTime; virtual;
    { Stores a TDateTime value. }
    procedure WriteDateTimeInt(const Path: string; Value: TDateTime); virtual;
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not a Boolean (or can't be converted to an Boolean an
      EConvertError exception will be raised. }
    function ReadBooleanInt(const Path: string; Default: Boolean = True): Boolean; virtual;
    { Stores a Boolean value. }
    procedure WriteBooleanInt(const Path: string; Value: Boolean); virtual;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function ReadBinaryInt(const Path: string; var Buf; BufSize: Integer): Integer; virtual; abstract;
    { Stores a buffer. }
    procedure WriteBinaryInt(const Path: string; const Buf; BufSize: Integer); virtual; abstract;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function ConcatPaths(const Paths: array of string): string;
    { Determines if the path represents a folder }
    function IsFolder(Path: string; ListIsValue: Boolean = True): Boolean; virtual; abstract;
    { Determines if the specified path exists }
    function PathExists(const Path: string): Boolean; virtual; abstract;
    { Determines if the specified value is stored }
    function ValueStored(const Path: string): Boolean; virtual; abstract;
    { Determines if the specified list is stored }
    function ListStored(const Path: string): Boolean; virtual;
    { Deletes the specified value. If the value wasn't stored, nothing will happen. }
    procedure DeleteValue(const Path: string); virtual; abstract;
    { Deletes all values and sub folders of the specified folder including the folder itself. }
    procedure DeleteSubTree(const Path: string); virtual; abstract;
    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function ReadInteger(const Path: string; Default: Integer = 0): Integer; virtual;
    { Stores an Integer value. }
    procedure WriteInteger(const Path: string; Value: Integer); virtual;
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised.}
    function ReadFloat(const Path: string; Default: Extended = 0): Extended; virtual;
    { Stores an Extended value. }
    procedure WriteFloat(const Path: string; Value: Extended); virtual;
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function ReadString(const Path: string; Default: string = ''): string; virtual;
    { Stores an string value. }
    procedure WriteString(const Path: string; Value: string); virtual;
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function ReadDateTime(const Path: string; Default: TDateTime = 0): TDateTime; virtual;
    { Stores a TDateTime value. }
    procedure WriteDateTime(const Path: string; Value: TDateTime); virtual;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function ReadBinary(const Path: string; var Buf; BufSize: Integer): Integer; virtual;
    { Stores a buffer. }
    procedure WriteBinary(const Path: string; const Buf; BufSize: Integer); virtual;
    { Retrieves the specified list. Caller provides a callback method that will read the individual
      items. ReadList will first determine the number of items to read and calls the specified
      method for each item. }
    function ReadList(const Path: string; const OnReadItem: TAppStoreListItem): Integer; virtual;
    { Stores a list of items. The number of items is stored first. For each item the provided
      item write method is called. Any additional items in the list (from a previous write) will be
      removed by the optionally provided delete method. }
    procedure WriteList(const Path: string; const ItemCount: Integer;
      const OnWriteItem: TAppStoreListItem;
      const OnDeleteItems: TAppStoreListDelete = nil); virtual;
    { Retrieves a string list. The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringList(const Path: string; const SL: TStrings;
      const ClearFirst: Boolean = True): Integer; virtual;
    { Stores a string list. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteStringList(const Path: string; const SL: TStrings); virtual;
    { Retrieves an enumeration. If the value is not found, the Default will be returned. }
    procedure ReadEnumeration(const Path: string; const TypeInfo: PTypeInfo;
      const Default; out Value); virtual;
    { Stores an enumeration }
    procedure WriteEnumeration(const Path: string; const TypeInfo: PTypeInfo;
      const Value); virtual;
    { Retrieves a set. If the value is not found, the Default will be returned. }
    procedure ReadSet(const Path: string; const ATypeInfo: PTypeInfo; const Default;
      out Value); virtual;
    { Stores a set. }
    procedure WriteSet(const Path: string; const ATypeInfo: PTypeInfo; const Value); virtual;
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not an Boolean (or can't be converted to a Boolean an EConvertError
      exception will be raised. }
    function ReadBoolean(const Path: string; Default: Boolean = True): Boolean; virtual;
    { Stores an Boolean value
      The value is stored as String TRUE/FALSE. }
    procedure WriteBoolean(const Path: string; Value: Boolean); virtual;
    { Retrieves a TPersistent-Object with all of its published properties }
    procedure ReadPersistent(const Path: string; const PersObj: TPersistent;
      const Recursive: Boolean = True; const ClearFirst: Boolean = True;
      const PropNameTranslator: TAppStorePropTranslate = nil);
    { Stores a TPersistent-Object with all of its published properties}
    procedure WritePersistent(const Path: string; const PersObj: TPersistent;
      const Recursive: Boolean = True; const IgnoreProperties: TStrings = nil;
      const PropNameTranslator: TAppStorePropTranslate = nil);

    { Translates a Char value to a (valid) key name. Used by the set storage methods. }
    function GetCharName(Ch: Char): string; virtual;
    { Translates an Integer value to a key name. Used by the set storage methods. }
    function GetIntName(Value: Integer): string; virtual;
    { Enumerate a list of stored values and/or folder below the specified path, optionally scanning
      sub folders as well. The associated object is an integer specifying what the string
      represents: 1: Folder; 2: Value; 3: Both }
    procedure GetStoredValues(const Path: string; const Strings: TStrings;
      const Options: TAppStoreEnumOptions = [aeoValues, aeoReportListAsValue, aeoRecursive]);
    { Root of any values to be read/written. This value is combined with the path given in one of
      the Read*/Write* methods to determine the actual key used. It's always relative to the value
      of Root (which is an absolute path) }
    property Path: string Read GetPath Write SetPath;
  published
    property StoreOptions: TJvAppStoreOptions Read fStoreOptions Write SetStoreOptions;
  end;

const
  aptFolder = 1;
  aptValue  = 2;

implementation

uses
  SysUtils,
  JclStrings, JclRTTI,
  JvTypes;

procedure UpdateGlobalPath(GlobalPaths, NewPaths: TStrings);
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to NewPaths.Count - 1 do
  begin
    if StrLeft(NewPaths[I], 1) = '.' then
    begin
      J := Length(NewPaths[I]) - 1;
      if J > GlobalPaths.Count then
        J := GlobalPaths.Count;
      while J > 0 do
      begin
        GlobalPaths.Delete(GlobalPaths.Count - 1);
        Dec(J);
      end;
    end
    else
      GlobalPaths.Add(NewPaths[I]);
  end;
end;

function OptimizePaths(Paths: array of string): string;
var
  GlobalPaths: TStrings;
  CurPaths: TStrings;
  Index: Integer;
begin
  if Length(Paths) <> 0 then
  begin
    GlobalPaths := nil;
    CurPaths := nil;
    try
      GlobalPaths := TStringList.Create;
      CurPaths := TStringList.Create;
      Index := High(Paths);
      while (Index > 0) and (StrLeft(Paths[Index], 1) <> '\') do
        Dec(Index);
      repeat
        StrToStrings(Paths[Index], '\', CurPaths, False);
        UpdateGlobalPath(GlobalPaths, CurPaths);
        Inc(Index);
      until Index > High(Paths);
      Result := StringsToStr(GlobalPaths, '\', False);
    finally
      CurPaths.Free;
      GlobalPaths.Free;
    end;
  end
  else
    Result := '';
end;

//===TJvAppStoreOptions=============================================================================

constructor TJvAppStoreOptions.Create;
begin
  inherited Create;
  BooleanStringTrueValues := 'TRUE, YES, Y';
  BooleanStringFalseValues := 'FALSE, NO, N';
  BooleanAsString := True;
  EnumerationAsString := True;
  SetAsString := False;
  DateTimeAsString := True;
  DefaultIfReadConvertError := False;
  DefaultIfValueNotExists := True;
end;

function TJvAppStoreOptions.IsValueListString(Value, List: string): Boolean;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.CommaText := Uppercase(List);
    Result := SL.IndexOf(Uppercase(Value)) >= 0;
  finally
    SL.Free;
  end;
end;

function TJvAppStoreOptions.DefaultTrueString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringTrueValues);
  if I = 0 then
    I := Length(FBooleanStringTrueValues) + 1;
  Result := Trim(Copy(FBooleanStringTrueValues, 1, I - 1));
end;

function TJvAppStoreOptions.DefaultFalseString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringFalseValues);
  if I = 0 then
    I := Length(FBooleanStringFalseValues) + 1;
  Result := Trim(Copy(FBooleanStringFalseValues, 1, I - 1));
end;

function TJvAppStoreOptions.IsValueTrueString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringTrueValues);
end;

function TJvAppStoreOptions.IsValueFalseString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringFalseValues);
end;

procedure TJvAppStoreOptions.SetBooleanAsString(Value: Boolean);
begin
  FBooleanAsString := Value and (DefaultTrueString <> '') and (DefaultFalseString <> '');
end;

procedure TJvAppStoreOptions.SetBooleanStringTrueValues(Value: string);
begin
  FBooleanStringTrueValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultTrueString <> '')
end;

procedure TJvAppStoreOptions.SetBooleanStringFalseValues(Value: string);
begin
  FBooleanStringFalseValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultFalseString <> '')
end;

procedure TJvAppStoreOptions.SetEnumAsStr(Value: Boolean);
begin
  FEnumAsStr := Value;
end;

procedure TJvAppStoreOptions.SetSetAsStr(Value: Boolean);
begin
  FSetAsStr := Value;
end;

procedure TJvAppStoreOptions.SetDateTimeAsStr(Value: Boolean);
begin
  FDateTimeAsString := Value;
end;

procedure TJvAppStoreOptions.SetFloatAsStr(Value: Boolean);
begin
  FFloatAsString := Value;
end;

procedure TJvAppStoreOptions.SetDefaultIfReadConvertError(Value: Boolean);
begin
  FDefaultIfReadConvertError := Value;
end;

procedure TJvAppStoreOptions.SetDefaultIfValueNotExists(Value: Boolean);
begin
  FDefaultIfValueNotExists := Value;
end;

//===TJvCustomAppStore==============================================================================

constructor TJvCustomAppStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoreOptions := TJvAppStoreOptions.Create;
end;

destructor TJvCustomAppStore.Destroy;
begin
  FreeAndNil(FStoreOptions);
  inherited Destroy;
end;

class function TJvCustomAppStore.GetStoreOptionsClass: TJvAppStoreOptionsClass;
begin
  Result := TJvAppStoreOptions;
end;

procedure TJvCustomAppStore.SplitKeyPath(const Path: string; out Key, ValueName: string);
var
  AbsPath:    string;
  IValueName: integer;
begin
  AbsPath := GetAbsPath(Path);
  IValueName := LastDelimiter('\', AbsPath);
  Key := StrLeft(AbsPath, IValueName - 1);
  ValueName := StrRestOf(AbsPath, IValueName + 1);
end;

function TJvCustomAppStore.GetRoot: string;
begin
  Result := FRoot;
end;

procedure TJvCustomAppStore.SetRoot(Value: string);
begin
  FRoot := OptimizePaths([Value]);
end;

function TJvCustomAppStore.GetCurrentPath: string;
begin
  Result := GetAbsPath('');
end;

function TJvCustomAppStore.GetAbsPath(Path: string): string;
begin
  Result := GetRoot + '\' + OptimizePaths([GetPath, Path]);
  while (Result <> '') and (Result[1] = '\') do
    Delete(Result, 1, 1);
end;

procedure TJvCustomAppStore.ReadSLItem(Sender: TJvCustomAppStore;
  const Path: string; const Index: Integer);
begin
  Sender.FStoreSL.Add(Sender.ReadString(Path + '\Item' + IntToStr(Index)));
end;

procedure TJvCustomAppStore.WriteSLItem(Sender: TJvCustomAppStore;
  const Path: string; const Index: Integer);
begin
  Sender.WriteString(Path + '\Item' + IntToStr(Index), Sender.FStoreSL[Index]);
end;

procedure TJvCustomAppStore.DeleteSLItems(Sender: TJvCustomAppStore;
  const Path: string; const First, Last: Integer);
var
  I: integer;
begin
  for I := First to Last do
    Sender.DeleteValue(Path + '\Item' + IntToStr(I));
end;

procedure TJvCustomAppStore.InternalGetStoredValues(const PrefixPath, SearchPath: string;
  const Strings: TStrings; const Options: TAppStoreEnumOptions);
var
  TempList: TStrings;
  I: Integer;
  S: string;
  PrevIdx: Integer;
begin
  TempList := TStringList.Create;
  try
    if (aeoValues in Options) then
    begin
      EnumValues(SearchPath, TempList, aeoReportListAsValue in Options);
      for I := 0 to TempList.Count - 1 do
      begin
        if TempList[I] = '' then
          S := Copy(PrefixPath, 1, Length(PrefixPath) - 1)
        else
          S := PrefixPath + TempList[I];
        if S <> '' then
        begin
          PrevIdx := Strings.IndexOf(S);
          if PrevIdx > -1 then
            Strings.Objects[PrevIdx] :=
              TObject(Integer(Strings.Objects[PrevIdx]) or aptValue)
          else
            Strings.AddObject(S, TObject(aptValue));
        end;
      end;
    end;
    if (aeoFolders in Options) or (aeoRecursive in Options) then
    begin
      TempList.Clear;
      EnumFolders(SearchPath, TempList, False);
      for I := 0 to TempList.Count - 1 do
      begin
        if (aeoFolders in Options) and IsFolder(SearchPath + '\' +
          TempList[I], aeoReportListAsValue in Options) then
        begin
          PrevIdx := Strings.IndexOf(PrefixPath + TempList[I]);
          if PrevIdx > -1 then
            Strings.Objects[PrevIdx] :=
              TObject(Integer(Strings.Objects[PrevIdx]) or aptFolder)
          else
            Strings.AddObject(PrefixPath + TempList[I], TObject(aptFolder));
        end;
        if (aeoRecursive in Options) then
          InternalGetStoredValues(PrefixPath + TempList[I] + '\',
            SearchPath + '\' + TempList[I],
            Strings, Options);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

function TJvCustomAppStore.GetPath: string;
begin
  Result := FCurPath;
end;

procedure TJvCustomAppStore.SetPath(const Path: string);
begin
  FCurPath := OptimizePaths([Path]);
end;

procedure TJvCustomAppStore.SetStoreOptions(Value: TJvAppStoreOptions);
begin
  if (Value <> nil) and (Value <> FStoreOptions) then
    FStoreOptions.Assign(Value);
end;

class function TJvCustomAppStore.NameIsListItem(Name: string): Boolean;
var
  NameStart: PChar;
begin
  NameStart := AnsiStrRScan(pchar(Name), '\');
  if NameStart = nil then
    NameStart := PChar(Name);
  Result := (AnsiStrLIComp(NameStart, 'Item', 4) = 0) and (NameStart[4] in ['0' .. '9']);
end;

class function TJvCustomAppStore.ConcatPaths(const Paths: array of string): string;
begin
  Result := OptimizePaths(Paths);
end;

function TJvCustomAppStore.ListStored(const Path: string): Boolean;
begin
  Result := ValueStored(Path + '\' + 'Count');
end;

function TJvCustomAppStore.ReadInteger(const Path: string; Default: Integer): Integer;
begin
  if not ValueStored(Path) and StoreOptions.DefaultIfValueNotExists then
  begin
    Result := Default;
    Exit;
  end;
  try
    Result := ReadIntegerInt(Path, Default);
  except
    on e: EConvertError do
      if StoreOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStore.WriteInteger(const Path: string; Value: Integer);
begin
  WriteIntegerInt(Path, Value);
end;

function TJvCustomAppStore.ReadFloat(const Path: string; Default: Extended): Extended;
begin
  if not ValueStored(Path) and StoreOptions.DefaultIfValueNotExists then
  begin
    Result := Default;
    Exit;
  end;
  try
    if StoreOptions.FloatAsString then
      try
        Result := StrToFloat(ReadString(Path));
      except
        on e: EConvertError do
          Result := ReadFloatInt(Path, Default);
      end
    else
      try
        Result := ReadFloatInt(Path, Default);
      except
        on e: EConvertError do
          Result := StrToFloat(ReadString(Path));
      end
  except
    on e: EConvertError do
      if StoreOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStore.WriteFloat(const Path: string; Value: Extended);
begin
  if StoreOptions.FloatAsString then
    WriteStringInt(Path, FloatToStr(Value))
  else
    WriteFloatInt(Path, Value);
end;

function TJvCustomAppStore.ReadString(const Path: string; Default: string): string;
begin
  if not ValueStored(Path) and StoreOptions.DefaultIfValueNotExists then
  begin
    Result := Default;
    Exit;
  end;
  try
    Result := ReadStringInt(Path, Default);
  except
    on e: EConvertError do
      if StoreOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStore.WriteString(const Path: string; Value: string);
begin
  WriteStringInt(Path, Value);
end;

function TJvCustomAppStore.ReadBinary(const Path: string; var Buf; BufSize: Integer): Integer;
begin
  Result := ReadBinaryInt(Path, Buf, BufSize);
end;

procedure TJvCustomAppStore.WriteBinary(const Path: string; const Buf; BufSize: Integer);
begin
  WriteBinaryInt(Path, Buf, BufSize);
end;

function TJvCustomAppStore.ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime;
begin
  Result := ReadFloat(Path, Default);
end;

procedure TJvCustomAppStore.WriteDateTimeInt(const Path: string; Value: TDateTime);
begin
  WriteFloat(Path, Value);
end;

function TJvCustomAppStore.ReadDateTime(const Path: string; Default: TDateTime): TDateTime;
begin
  if not ValueStored(Path) and StoreOptions.DefaultIfValueNotExists then
  begin
    Result := Default;
    Exit;
  end;
  try
    if StoreOptions.DateTimeAsString then
      try
        Result := StrToDateTime(ReadString(Path, DateTimeToStr(Default)));
      except
        on e: EConvertError do
          Result := ReadDateTimeInt(Path, Default);
      end
    else
      try
        Result := ReadDateTimeInt(Path, Default);
      except
        on e: EConvertError do
          Result := StrToDateTime(ReadString(Path, DateTimeToStr(Default)));
      end
  except
    on e: EConvertError do
      if StoreOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStore.WriteDateTime(const Path: string; Value: TDateTime);
begin
  if StoreOptions.DateTimeAsString then
    WriteString(Path, DateTimeToStr(Value))
  else
    WriteDateTimeInt(Path, Value);
end;

function TJvCustomAppStore.ReadBooleanInt(const Path: string; Default: Boolean): Boolean;
var
  Value: Integer;
begin
  Result := Default;
  Value := ReadInteger(Path, Ord(Default));
  if Value = Ord(True) then
    Result := True
  else if Value = Ord(False) then
    Result := False
  else
    EConvertError.Create('Invalid Boolean : ' + IntToStr(Value));
end;

procedure TJvCustomAppStore.WriteBooleanInt(const Path: string; Value: Boolean);
begin
  WriteInteger(Path, Ord(Value));
end;

function TJvCustomAppStore.ReadBoolean(const Path: string; Default: Boolean): Boolean;
var
  Value: string;
begin
  if not ValueStored(Path) and StoreOptions.DefaultIfValueNotExists then
  begin
    Result := Default;
    Exit;
  end;
  try
    if StoreOptions.BooleanAsString then
    begin
      Value := ReadString(Path);
      if StoreOptions.IsValueTrueString(Value) then
        Result := true
      else if StoreOptions.IsValueFalseString(Value) then
        Result := false
      else
        Result := ReadBooleanInt(Path, Default);
    end
    else
      Result := ReadBooleanInt(Path, Default);
  except
    on e: EConvertError do
      if StoreOptions.DefaultIfReadConvertError then
        Result := Default
      else
        raise;
  end;
end;

procedure TJvCustomAppStore.WriteBoolean(const Path: string; Value: Boolean);
begin
  if StoreOptions.BooleanAsString then
    if Value then
      WriteString(Path, StoreOptions.DefaultTrueString)
    else
      WriteString(Path, StoreOptions.DefaultFalseString)
  else
    WriteBooleanInt(Path, Value);
end;

function TJvCustomAppStore.ReadList(const Path: string;
  const OnReadItem: TAppStoreListItem): Integer;
var
  I: Integer;
begin
  Result := ReadInteger(Path + '\Count');
  for I := 0 to Result - 1 do
    OnReadItem(Self, Path, I);
end;

procedure TJvCustomAppStore.WriteList(const Path: string; const ItemCount: Integer;
  const OnWriteItem: TAppStoreListItem; const OnDeleteItems: TAppStoreListDelete);
var
  PrevListCount: Integer;
  I: Integer;
begin
  PrevListCount := ReadInteger(Path + '\Count');
  WriteInteger(Path + '\Count', ItemCount);
  for I := 0 to ItemCount - 1 do
    OnWriteItem(Self, Path, I);
  if (PrevListCount > ItemCount) and Assigned(OnDeleteItems) then
    OnDeleteItems(Self, Path, ItemCount, PrevListCount - 1);
end;

function TJvCustomAppStore.ReadStringList(const Path: string; const SL: TStrings;
  const ClearFirst: Boolean): Integer;
begin
  if ClearFirst then
    SL.Clear;
  FStoreSL := SL;
  Result   := ReadList(Path, ReadSLItem);
end;

procedure TJvCustomAppStore.WriteStringList(const Path: string; const SL: TStrings);
begin
  FStoreSL := SL;
  WriteList(Path, SL.Count, WriteSLItem, DeleteSLItems);
end;

procedure CopyEnumValue(const Source; var Target; const Kind: TOrdType);
begin
  case Kind of
    otSByte,
    otUByte:
      Byte(Target) := Byte(Source);
    otSWord,
    otUWord:
      Word(Target) := Word(Source);
    otSLong,
    otULong:
      Longword(Target) := Longword(Source);
  end;
end;

procedure TJvCustomAppStore.ReadEnumeration(const Path: string; const TypeInfo: PTypeInfo;
  const Default; out Value);
var
  OrdValue: Integer;
begin
  if TypeInfo.Kind <> tkEnumeration then
    raise EJVCLException.Create('Not an enumeration type.');
  if not ValueStored(Path) and StoreOptions.DefaultIfValueNotExists then
  begin
    CopyEnumValue(Default, Value, GetTypeData(TypeInfo).OrdType);
    Exit;
  end;
  try
    // Usage of an invalid identifier to signal the value does not exist
    OrdValue := GetEnumValue(TypeInfo, ReadString(Path, ' #!@not known@!# '));
    if OrdValue = -1 then
    begin
      // Invalid string or not string found; try as Integer instead
      OrdValue := 0;
      CopyEnumValue(Default, OrdValue, GetTypeData(TypeInfo).OrdType);
      OrdValue := ReadInteger(Path, OrdValue);
    end
  except
    on E: EConvertError do
      if StoreOptions.DefaultIfReadConvertError then
        CopyEnumValue(Default, Value, GetTypeData(TypeInfo).OrdType)
      else
        raise;
  end;
  CopyEnumValue(OrdValue, Value, GetTypeData(TypeInfo).OrdType);
end;

function OrdOfEnum(const Value; OrdType: TOrdType): Integer;
begin
  case OrdType of
    otSByte:
      Result := ShortInt(Value);
    otUByte:
      Result := Byte(Value);
    otSWord:
      Result := SmallInt(Value);
    otUWord:
      Result := Word(Value);
    otSLong,
    otULong:
      Result := LongInt(Value);
    else
      Result := -1;
  end;
end;

procedure TJvCustomAppStore.WriteEnumeration(const Path: string; const TypeInfo: PTypeInfo;
  const Value);
begin
  if StoreOptions.EnumerationAsString then
    WriteString(Path, GetEnumName(TypeInfo, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType)))
  else
    WriteInteger(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
end;

procedure TJvCustomAppStore.ReadSet(const Path: string; const ATypeInfo: PTypeInfo; const Default;
  out Value);
var
  Lst: TStrings;
  I: Integer;
begin
  if PathExists(Path) then
  begin
    Lst := TStringList.Create;
    try
      with (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as IJclOrdinalRangeTypeInfo do
      begin
        case GetTypeKind of
          tkEnumeration:
            begin
              with ((JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as
                  IJclEnumerationTypeInfo) do
                for I := GetMinValue to GetMaxValue do
                  if ReadBoolean(ConcatPaths([Path, GetNames(I)]), False) then
                    Lst.Add(GetNames(I));
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).SetAsList(Value, Lst);
            end;
          tkChar:
            begin
              JclStrToSet(ATypeInfo, Value, ''); // empty out value
              for I := GetMinValue to GetMaxValue do
                if ReadBoolean(ConcatPaths([Path, GetCharName(Chr(I))]), False) then
                  Include(TIntegerSet(Value), I);
            end;
          tkInteger:
            begin
              for I := GetMinValue to GetMaxValue do
                if ReadBoolean(ConcatPaths([Path, GetIntName(I)]), False) then
                  Lst.Add(IntToStr(I));
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).SetAsList(Value, Lst);
            end;
          else
            raise EJVCLException.Create('Unknown base type for given set.');
        end;
      end;
    finally
      FreeAndNil(Lst);
    end;
  end
  else // It's stored as a string value or not stored at all
    JclStrToSet(ATypeInfo, Value, ReadString(Path, JclSetToStr(ATypeInfo, Default, True)));
end;

procedure TJvCustomAppStore.WriteSet(const Path: string; const ATypeInfo: PTypeInfo; const Value);
var
  Lst: TStrings;
  I: Integer;
begin
  if StoreOptions.SetAsString then
    WriteString(Path, JclSetToStr(ATypeInfo, Value, True))
  else
  begin
    Lst := TStringList.Create;
    try
      with (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as IJclOrdinalRangeTypeInfo do
      begin
        case GetTypeKind of
          tkEnumeration:
            begin
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).GetAsList(Value, False, Lst);
              with ((JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as
                  IJclEnumerationTypeInfo) do
                for I := GetMinValue to GetMaxValue do
                  WriteBoolean(ConcatPaths([Path, GetNames(I)]),
                    Lst.IndexOf(GetNames(I)) > - 1);
            end;
          tkChar:
            begin
              for I := GetMinValue to GetMaxValue do
                WriteBoolean(ConcatPaths([Path, GetCharName(Chr(I))]), I in TIntegerSet(Value));
            end;
          tkInteger:
            begin
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).GetAsList(Value, False, Lst);
              for I := GetMinValue to GetMaxValue do
                WriteBoolean(ConcatPaths([Path, GetIntName(I)]),
                  Lst.IndexOf(IntToStr(I)) > - 1);
            end;
          else
            raise EJVCLException.Create('Unknown base type for given set.');
        end;
      end;
    finally
      FreeAndNil(Lst);
    end;
  end;
  WriteString(Path, JclSetToStr(ATypeInfo, Value, True));
end;

function TJvCustomAppStore.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data   := GetTypeData(Instance.Classinfo);
  Result := Data^.PropCount;
end;

function TJvCustomAppStore.GetPropName(Instance: TPersistent; Index: Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  GetMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  end;
end;

procedure TJvCustomAppStore.ReadPersistent(const Path: string; const PersObj: TPersistent;
  const Recursive, ClearFirst: Boolean; const PropNameTranslator: TAppStorePropTranslate);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  TmpValue: Integer;
begin
  if not Assigned(PersObj) then
    Exit;
  for Index := 0 to GetPropCount(PersObj) - 1 do
  begin
    PropName := GetPropName(PersObj, Index);
    KeyName := PropName;
    if @PropNameTranslator <> nil then
      PropNameTranslator(Self, PersObj, KeyName, True);
    PropPath := ConcatPaths([Path, KeyName]);
    case PropType(PersObj, PropName) of
      tkLString,
      tkWString,
      tkString:
        SetStrProp(PersObj, PropName, ReadString(PropPath, GetStrProp(PersObj, PropName)));
      tkEnumeration:
        begin
          TmpValue := GetOrdProp(PersObj, PropName);
          ReadEnumeration(PropPath, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
          SetOrdProp(PersObj, PropName, TmpValue);
        end;
      tkSet:
        begin
          TmpValue := GetOrdProp(PersObj, PropName);
          ReadSet(PropPath, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
          SetOrdProp(PersObj, PropName, TmpValue);
        end;
      tkChar,
      tkInteger:
        WriteInteger(PropPath, GetOrdProp(PersObj, PropName));
      tkInt64:
        WriteString(PropPath, IntToStr(GetInt64Prop(PersObj, PropName)));
      tkFloat:
        WriteFloat(PropPath, GetFloatProp(PersObj, PropName));
      tkClass:
        begin
          if (TPersistent(GetOrdProp(PersObj, PropName)) is TStrings) then
            ReadStringList(PropPath, TStrings(GetOrdProp(PersObj, PropName)), True)
          else if (TPersistent(GetOrdProp(PersObj, PropName)) is TPersistent) and Recursive then
            ReadPersistent(PropPath, TPersistent(GetOrdProp(PersObj, PropName)), True);
        end;
    end;
  end;
end;

procedure TJvCustomAppStore.WritePersistent(const Path: string; const PersObj: TPersistent;
  const Recursive: Boolean; const IgnoreProperties: TStrings;
  const PropNameTranslator: TAppStorePropTranslate);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  TmpValue: Integer;
begin
  if not Assigned(PersObj) then
    Exit;
  for Index := 0 to GetPropCount(PersObj) - 1 do
  begin
    PropName := GetPropName(PersObj, Index);
    KeyName := PropName;
    if @PropNameTranslator <> nil then
      PropNameTranslator(Self, PersObj, KeyName, False);
    PropPath := ConcatPaths([Path, KeyName]);
    if (IgnoreProperties = nil) or (IgnoreProperties.IndexOf(PropName) = -1) then
      case PropType(PersObj, PropName) of
        tkLString,
        tkWString,
        tkString:
          WriteString(PropPath, GetStrProp(PersObj, PropName));
        tkEnumeration:
          begin
            TmpValue := GetOrdProp(PersObj, PropName);
            WriteEnumeration(PropPath, GetPropInfo(PersObj, PropName).PropType^, TmpValue);
          end;
        tkSet:
          begin
            TmpValue := GetOrdProp(PersObj, PropName);
            WriteSet(PropPath, GetPropInfo(PersObj, PropName).PropType^, TmpValue);
          end;
        tkChar,
        tkInteger:
          WriteInteger(PropPath, GetOrdProp(PersObj, PropName));
        tkInt64:
          WriteString(PropPath, IntToStr(GetInt64Prop(PersObj, PropName)));
        tkFloat:
          WriteFloat(PropPath, GetFloatProp(PersObj, PropName));
        tkClass:
        begin
          if (TPersistent(GetOrdProp(PersObj, PropName)) is TStrings) then
            WriteStringList(PropPath, TStrings(GetOrdProp(PersObj, PropName)))
          else if (TPersistent(GetOrdProp(PersObj, PropName)) is TPersistent) and Recursive then
            WritePersistent(PropPath, TPersistent(GetOrdProp(PersObj, PropName)), True);
        end;
      end;
  end;
end;

function TJvCustomAppStore.GetCharName(Ch: Char): string;
begin
  if Ch in ['!' .. 'z'] then
    Result := 'Char_' + Ch
  else
    Result := 'Char#' + IntToStr(Ord(Ch));
end;

function TJvCustomAppStore.GetIntName(Value: Integer): string;
begin
  Result := 'Int_' + IntToStr(Value);
end;

procedure TJvCustomAppStore.GetStoredValues(const Path: string;
  const Strings: TStrings; const Options: TAppStoreEnumOptions);
var
  SearchPath: string;
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    SearchPath := OptimizePaths([Path]);
    if aeoReportRelative in Options then
      InternalGetStoredValues('', SearchPath, Strings, Options)
    else
      InternalGetStoredValues(OptimizePaths([Self.Path, SearchPath]) +
        '\', SearchPath, Strings,
        Options);
    I := Strings.IndexOf(OptimizePaths([Self.Path, SearchPath]));
    if I > -1 then
      Strings.Delete(I);
  finally
    Strings.EndUpdate;
  end;
end;

end.
