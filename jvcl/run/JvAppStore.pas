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

  Storage hierarchies
  ===================
  Each storage allows you add an unlimited number of sub storages. A sub storage is a symbolic
  link between a path in a storage to another storage (which in turn can also provide sub storages).

  Suppose you want to store both generic as well as user specific settings. This can be accomplished
  with two stores, one for the generic settings and one specific for the current user. The generic
  store (referred to as 'asRegBackend' from now on) will link to the user specific store (referred
  to as 'asUserIniBackend' from now on) using asRegBackend.SubStores. The RootPath for the
  asUserIniBackend sub-store link will be set to 'UserSettings'. From that point on, any reference
  to a sub path of '\UserSettings' from the asRegBackend storage will be handed over to the
  asUserIniBackend storage. Examples:

  Path                          Target
  ====                          ======
  \WinPath                      asRegBackend:'\WinPath'
  \Generic\UserSettings\Me      asRegBackend:'\Generic\UserSettings\Me'
  \UserSettings                 asRegBackend:'\UserSettings'
  \UserSettings\FirstName       asUserIniBackend:'\FirstName'
  \UserSettings\Sub1\Sub1.1     asUserIniBackend:'\Sub1\Sub1.1'

  Because all settings can be read from a single store (from the application's perspective) you have
  created the option to keep your settings storage and retrieval code simple and easy to understand.
  Upon startup you can set asUserIniBackend to the correct INI file for the user that has logged on,
  and you are ready to read in the settings of that user
}

interface

uses
  Classes, TypInfo,
  JvComponent, JvTypes;

type
  TJvCustomAppStore = class;
  TJvAppStore = class;
  TJvCustomAppStoreOptions = class;
  TJvAppSubStores = class;
  TJvAppSubStore = class;

  EJVCLAppStoreError = class(EJVCLException);

  TAppStoreListItemEvent = procedure(Sender: TJvCustomAppStore; const Path: string;
    const Index: Integer) of object;
  TAppStoreListDeleteEvent = procedure(Sender: TJvCustomAppStore; const Path: string;
    const First, Last: Integer) of object;
  TAppStorePropTranslateEvent = procedure(Sender: TJvCustomAppStore; Instance: TPersistent;
    var Name: string; const Reading: Boolean) of object;

  TJvAppStoreOptionsClass = class of TJvCustomAppStoreOptions;

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
    FStoreOptions: TJvCustomAppStoreOptions;
    FSubStores: TJvAppSubStores;
    FOnTranslatePropertyName: TAppStorePropTranslateEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //Returns the property count of an instance
    function GetPropCount(Instance: TPersistent): Integer;
    //Returns the property name of an instance at a certain index
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetStoreOptionsClass: TJvAppStoreOptionsClass; virtual;
    { Split the specified path into an absolute path and a value name (the last item in the path
      string). Just a helper for all the storage methods. }
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); virtual;
    { SubStores property set method. Does nothing. }
    procedure SetSubStores(Value: TJvAppSubStores);
    { Retrieve application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    function GetRoot: string;
    { Set application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    procedure SetRoot(Value: string);
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
    function GetPath: string;
    { Specify a new root. Given path is relative to the current path. Se remarks above }
    procedure SetPath(const Path: string);
    { Determines if the specified name belongs to a list value. }
    class function NameIsListItem(Name: string): Boolean;
    { Application specific root. Path is prepended to any specified path and serves as an absolute
      root for any reading/writing. Not all implementation will use it. Generally it's used for
      storages not specific to an application (such as the registry). }
    property Root: string Read GetRoot Write SetRoot;
    { Set the StoreOptions Property }
    procedure SetStoreOptions(Value: TJvCustomAppStoreOptions);
    { Invokes the OnTranslatePropertyName event if one is assigned. }
    procedure DoTranslatePropertyName(Instance: TPersistent; var Name: string;
      const Reading: Boolean);
    { Determines if the specified is a sub store of this storage (will scan the entire sub storage
      hierarchie. }
    function HasSubStore(AStore: TJvCustomAppStore): Boolean;

    { Determines if the path represents a folder (ignores sub stores) }
    function IsFolderInt(Path: string; ListIsValue: Boolean = True): Boolean; virtual; abstract;
    { Determines if the specified path exists (ignores sub stores) }
    function PathExistsInt(const Path: string): Boolean; virtual; abstract;
    { Determines if the specified value is stored (ignores sub stores) }
    function ValueStoredInt(const Path: string): Boolean; virtual; abstract;
    { Determines if the specified list is stored (ignores sub stores) }
    function ListStoredInt(const Path: string): Boolean;
    { Deletes the specified value. If the value wasn't stored, nothing will happen (ignores sub
      stores). }
    procedure DeleteValueInt(const Path: string); virtual; abstract;
    { Deletes all values and sub folders of the specified folder including the folder itself
      (ignores sub stores). }
    procedure DeleteSubTreeInt(const Path: string); virtual; abstract;
    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function DoReadInteger(const Path: string; Default: Integer): Integer; virtual; abstract;
    { Stores an Integer value. }
    procedure DoWriteInteger(const Path: string; Value: Integer); virtual; abstract;
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised.}
    function DoReadFloat(const Path: string; Default: Extended): Extended; virtual; abstract;
    { Stores an Extended value. }
    procedure DoWriteFloat(const Path: string; Value: Extended); virtual; abstract;
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function DoReadString(const Path: string; Default: string): string; virtual; abstract;
    { Stores an string value. }
    procedure DoWriteString(const Path: string; Value: string); virtual; abstract;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function DoReadBinary(const Path: string; var Buf; BufSize: Integer): Integer; virtual; abstract;
    { Stores a buffer. }
    procedure DoWriteBinary(const Path: string; const Buf; BufSize: Integer); virtual; abstract;
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function DoReadDateTime(const Path: string; Default: TDateTime): TDateTime; virtual;
    { Stores a TDateTime value (ignores sub stores). }
    procedure DoWriteDateTime(const Path: string; Value: TDateTime); virtual;
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not a Boolean (or can't be converted to an Boolean an
      EConvertError exception will be raised. }
    function DoReadBoolean(const Path: string; Default: Boolean): Boolean; virtual;
    { Stores a Boolean value. }
    procedure DoWriteBoolean(const Path: string; Value: Boolean); virtual;

    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function ReadIntegerInt(const Path: string; Default: Integer): Integer; virtual;
    { Stores an Integer value (ignores sub stores). }
    procedure WriteIntegerInt(const Path: string; Value: Integer); virtual;
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised (ignores sub stores). }
    function ReadFloatInt(const Path: string; Default: Extended): Extended; virtual;
    { Stores an Extended value (ignores sub stores). }
    procedure WriteFloatInt(const Path: string; Value: Extended); virtual;
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised (ignores sub stores). }
    function ReadStringInt(const Path: string; Default: string): string; virtual;
    { Stores an string value (ignores sub stores). }
    procedure WriteStringInt(const Path: string; Value: string); virtual;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved (ignores sub stores). }
    function ReadBinaryInt(const Path: string; var Buf; BufSize: Integer): Integer; virtual;
    { Stores a buffer (ignores sub stores). }
    procedure WriteBinaryInt(const Path: string; const Buf; BufSize: Integer); virtual;
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised (ignores sub stores). }
    function ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime; virtual;
    { Stores a TDateTime value (ignores sub stores). }
    procedure WriteDateTimeInt(const Path: string; Value: TDateTime); virtual;
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not a Boolean (or can't be converted to an Boolean an
      EConvertError exception will be raised (ignores sub stores). }
    function ReadBooleanInt(const Path: string; Default: Boolean): Boolean; virtual;
    { Stores a Boolean value (ignores sub stores). }
    procedure WriteBooleanInt(const Path: string; Value: Boolean); virtual;
    { Retrieves an enumeration. If the value is not found, the Default will be returned (ignores sub
      stores). }
    procedure ReadEnumerationInt(const Path: string; const TypeInfo: PTypeInfo; const Default;
      out Value); virtual;
    { Stores an enumeration (ignores sub stores). }
    procedure WriteEnumerationInt(const Path: string; const TypeInfo: PTypeInfo;
      const Value); virtual;
    { Retrieves a set. If the value is not found, the Default will be returned (ignores sub
      stores). }
    procedure ReadSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Default;
      out Value); virtual;
    { Stores a set (ignores sub stores). }
    procedure WriteSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Value); virtual;

    property SubStores: TJvAppSubStores read FSubStores write SetSubStores;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function ConcatPaths(const Paths: array of string): string;
    { Resolve a path to it's actual used storage backend and root path. }
    procedure ResolvePath(InPath: string; out TgtStore: TJvCustomAppStore; out TgtPath: string);
    { Determines if the path represents a folder }
    function IsFolder(Path: string; ListIsValue: Boolean = True): Boolean;
    { Determines if the specified path exists }
    function PathExists(const Path: string): Boolean;
    { Determines if the specified value is stored }
    function ValueStored(const Path: string): Boolean;
    { Determines if the specified list is stored }
    function ListStored(const Path: string): Boolean;
    { Deletes the specified value. If the value wasn't stored, nothing will happen. }
    procedure DeleteValue(const Path: string);
    { Deletes all values and sub folders of the specified folder including the folder itself. }
    procedure DeleteSubTree(const Path: string);
    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function ReadInteger(const Path: string; Default: Integer = 0): Integer;
    { Stores an Integer value. }
    procedure WriteInteger(const Path: string; Value: Integer);
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised.}
    function ReadFloat(const Path: string; Default: Extended = 0): Extended;
    { Stores an Extended value. }
    procedure WriteFloat(const Path: string; Value: Extended);
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function ReadString(const Path: string; Default: string = ''): string;
    { Stores an string value. }
    procedure WriteString(const Path: string; Value: string);
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function ReadDateTime(const Path: string; Default: TDateTime = 0): TDateTime;
    { Stores a TDateTime value. }
    procedure WriteDateTime(const Path: string; Value: TDateTime);
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function ReadBinary(const Path: string; var Buf; BufSize: Integer): Integer;
    { Stores a buffer. }
    procedure WriteBinary(const Path: string; const Buf; BufSize: Integer);
    { Retrieves the specified list. Caller provides a callback method that will read the individual
      items. ReadList will first determine the number of items to read and calls the specified
      method for each item. }
    function ReadList(const Path: string;
      const OnReadItem: TAppStoreListItemEvent): Integer;
    { Stores a list of items. The number of items is stored first. For each item the provided
      item write method is called. Any additional items in the list (from a previous write) will be
      removed by the optionally provided delete method. }
    procedure WriteList(const Path: string; const ItemCount: Integer;
      const OnWriteItem: TAppStoreListItemEvent;
      const OnDeleteItems: TAppStoreListDeleteEvent = nil);
    { Retrieves a string list. The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringList(const Path: string; const SL: TStrings;
      const ClearFirst: Boolean = True): Integer;
    { Stores a string list. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteStringList(const Path: string; const SL: TStrings);
    { Retrieves an enumeration. If the value is not found, the Default will be returned. }
    procedure ReadEnumeration(const Path: string; const TypeInfo: PTypeInfo;
      const Default; out Value);
    { Stores an enumeration }
    procedure WriteEnumeration(const Path: string; const TypeInfo: PTypeInfo;
      const Value);
    { Retrieves a set. If the value is not found, the Default will be returned. }
    procedure ReadSet(const Path: string; const ATypeInfo: PTypeInfo; const Default; out Value);
    { Stores a set. }
    procedure WriteSet(const Path: string; const ATypeInfo: PTypeInfo; const Value);
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not an Boolean (or can't be converted to a Boolean an EConvertError
      exception will be raised. }
    function ReadBoolean(const Path: string; Default: Boolean = True): Boolean;
    { Stores an Boolean value
      The value is stored as String TRUE/FALSE. }
    procedure WriteBoolean(const Path: string; Value: Boolean);
    { Retrieves a TPersistent-Object with all of its published properties }
    procedure ReadPersistent(const Path: string; const PersObj: TPersistent;
      const Recursive: Boolean = True; const ClearFirst: Boolean = True);
    { Stores a TPersistent-Object with all of its published properties}
    procedure WritePersistent(const Path: string; const PersObj: TPersistent;
      const Recursive: Boolean = True; const IgnoreProperties: TStrings = nil);

    { Translates a Char value to a (valid) key name. Used by the set storage methods. }
    function GetCharName(Ch: Char): string; virtual;
    { Translates an Integer value to a key name. Used by the set storage methods. }
    function GetIntName(Value: Integer): string; virtual;
    { Translates between a property name and it's storage name. If Reading is True, AName is
      interpreted as a storage name to be translated to a real property name. If Reading is False,
      AName is interpreted as a property name to be translated to a storage name. Will invoke the
      OnTranslatePropertyName event if one is assigned, or return AName if no handler is assigned. }
    function TranslatePropertyName(Instance: TPersistent; const AName: string;
      const Reading: Boolean): string;
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
    property StoreOptions: TJvCustomAppStoreOptions read FStoreOptions write SetStoreOptions;
    property OnTranslatePropertyName: TAppStorePropTranslateEvent read FOnTranslatePropertyName
      write FOnTranslatePropertyName;
  end;

  { Generic store that can only be used to combine various other storages (only storages in the
    SubStores collection are usable; any references to paths not specified in this collection
    will raise an exception). Can be used for example to provide access to the entire registry
    hive from a single app store component by adding a number of TJvAppRegistryStore storages,
    each referencing a specific root key and link them to a suitable root key path:

    RootPath              Store
    ========              =====
    HKCR                  asRegStoreHKCR
    HKEY_CLASSES_ROOT     asRegStoreHKCR
    HKCU                  asRegStoreHKCU
    HKEY_CURRENT_USER     asRegStoreHKCU
    HKLM                  asRegStoreHKLM
    HKEY_LOCAL_MACHINE    asRegStoreHKLM

    In the above scheme, both 'HKCU\<path>' as well as 'HKEY_CURRENT_USER'<path>' will link to
    asRegStoreHKCU, ie. HKCU and HKEY_CURRENT_USER are aliases of each other. }
  TJvAppStore = class(TJvCustomAppStore)
  protected
    function IsFolderInt(Path: string; ListIsValue: Boolean = True): Boolean; override;
    function PathExistsInt(const Path: string): Boolean; override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    procedure DeleteSubTreeInt(const Path: string); override;
    function ReadIntegerInt(const Path: string; Default: Integer = 0): Integer; override;
    procedure WriteIntegerInt(const Path: string; Value: Integer); override;
    function ReadFloatInt(const Path: string; Default: Extended = 0): Extended; override;
    procedure WriteFloatInt(const Path: string; Value: Extended); override;
    function ReadStringInt(const Path: string; Default: string = ''): string; override;
    procedure WriteStringInt(const Path: string; Value: string); override;
    function ReadBinaryInt(const Path: string; var Buf; BufSize: Integer): Integer; override;
    procedure WriteBinaryInt(const Path: string; const Buf; BufSize: Integer); override;
    function ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime; override;
    procedure WriteDateTimeInt(const Path: string; Value: TDateTime); override;
    function ReadBooleanInt(const Path: string; Default: Boolean): Boolean; override;
    procedure WriteBooleanInt(const Path: string; Value: Boolean); override;
    procedure ReadEnumerationInt(const Path: string; const TypeInfo: PTypeInfo; const Default;
      out Value); override;
    procedure WriteEnumerationInt(const Path: string; const TypeInfo: PTypeInfo;
      const Value); override;
    procedure ReadSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Default;
      out Value); override;
    procedure WriteSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Value); override;
  published
    property SubStores;
  end;

  TJvCustomAppStoreOptions = class(TPersistent)
  private
    FBooleanAsString: Boolean;
    FBooleanStringTrueValues: string;
    FBooleanStringFalseValues: string;
    FEnumAsStr: Boolean;
    FIntAsStr: Boolean;
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
    procedure SetIntAsStr(Value: Boolean); virtual;
    procedure SetSetAsStr(Value: Boolean); virtual;
    procedure SetDateTimeAsStr(Value: Boolean); virtual;
    procedure SetFloatAsStr(Value: Boolean); virtual;
    procedure SetDefaultIfReadConvertError(Value: Boolean); virtual;
    procedure SetDefaultIfValueNotExists(Value: Boolean); virtual;
    function IsValueListString(AValue, AList: string): Boolean; virtual;
  public
    constructor Create;
    function DefaultTrueString: string;
    function DefaultFalseString: string;
    function IsValueTrueString(Value: string): Boolean;
    function IsValueFalseString(Value: string): Boolean;

    property BooleanStringTrueValues: string read FBooleanStringTrueValues
      write SetBooleanStringTrueValues;
    property BooleanStringFalseValues: string read FBooleanStringFalseValues
      write SetBooleanStringFalseValues;
    property BooleanAsString: Boolean read FBooleanAsString write SetBooleanAsString default True;
    property EnumerationAsString: Boolean read FEnumAsStr write SetEnumAsStr default True;
    property TypedIntegerAsString: Boolean read FIntAsStr write SetIntAsStr default True;
    property SetAsString: Boolean read FSetAsStr write SetSetAsStr default False;
    property DateTimeAsString: Boolean read FDateTimeAsString write SetDateTimeAsStr default True;
    property FloatAsString: Boolean read FFloatAsString write SetFloatAsStr default False;
    property DefaultIfReadConvertError: Boolean read FDefaultIfReadConvertError
      write SetDefaultIfReadConvertError default False;
    property DefaultIfValueNotExists: Boolean read FDefaultIfValueNotExists
      write SetDefaultIfValueNotExists default True;
  end;

  TJvAppStoreOptions = class(TJvCustomAppStoreOptions)
  published
    property BooleanStringTrueValues;
    property BooleanStringFalseValues;
    property BooleanAsString;
    property EnumerationAsString;
    property TypedIntegerAsString;
    property SetAsString;
    property DateTimeAsString;
    property FloatAsString;
    property DefaultIfReadConvertError;
    property DefaultIfValueNotExists;
  end;

  TJvAppSubStores = class(TOwnedCollection)
  private
    function GetRootStorage: TJvCustomAppStore;
    function GetItem(I: Integer): TJvAppSubStore;
    procedure SetItem(I: Integer; Value: TJvAppSubStore);
  protected
    { Notify sub storages of a change in the options of the root storage. This allows sub storage
      to be kept in sync with the root storage. }
    procedure RootOptionsChanged;
    { Check if the given root path is unique, optionally ignoring a specific sub storage (eg. when
      modifying the root path of a storage, that storage's RootPath is irrelavant in determining
      if the new name will be unique). }
    function CheckUniqueBase(APath: string; IgnoreIndex: Integer): Boolean;
    { Retrieves the sub storage for the given root path, optionally ignoring a specific sub storage.
      The specified path is assumed to be at root level (regardless whether the paths starts with
      a backslash (\) or not) and leading and trailing backslashes are removed automatically.
      The last element in the path string is ignored to avoid returning a sub storage for the root
      path itself. To search for a sub store for a root path, simply add '\*' at the end of the
      path. }
    function MatchFor(APath: string; IgnoreIndex: Integer = -1): TJvAppSubStore;

    property RootStorage: TJvCustomAppStore read GetRootStorage;
  public
    constructor Create(AOwner: TJvCustomAppStore);
    procedure Add(RootPath: string; AppStore: TJvCustomAppStore);
    procedure Delete(Index: Integer); overload;
    procedure Delete(RootPath: string; const IncludeSubPaths: Boolean = False); overload;
    procedure Delete(AppStore: TJvCustomAppStore); overload;

    property Items[I: Integer]: TJvAppSubStore read GetItem write SetItem; default;
  end;

  TJvAppSubStore = class(TCollectionItem)
  private
    FRootPath: string;
    FAppStore: TJvCustomAppStore;
  protected
    function GetOwnerStore: TJvCustomAppStore;
    function GetDisplayName: string; override;
    procedure SetRootPath(Value: string);
    procedure SetAppStore(Value: TJvCustomAppStore);

    property OwnerStore: TJvCustomAppStore read GetOwnerStore;
  published
    property RootPath: string read FRootPath write SetRootPath;
    property AppStore: TJvCustomAppStore read FAppStore write SetAppStore;
  end;

// (marcelb) moved back; the constants are useful to the outside world after a call to GetStoredValues
// (rom) give it better names and delete these comments :-)
const
  aptFolder = 1;
  aptValue  = 2;

implementation

uses
  SysUtils,
  JclStrings, JclRTTI,
  JvPropertyStore;

const
  // (rom) this name is shared in several units and should be made global
  cCount = 'Count';
  cItem = 'Item';
  cInvalidIdentifier = ' #!@not known@!# ';

resourcestring
  SInvalidType = 'Invalid type';
  SUnknownBaseType = 'Unknown base type for given set';
  SInvalidPath = 'Invalid path';
  SNotAUniqueRootPath = '''%s'' is not a unique root path';
  SCircularReferenceOfStorages = 'Circular reference of storages';

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

procedure CopyEnumValue(const Source; var Target; const Kind: TOrdType);
begin
  case Kind of
    otSByte, otUByte:
      Byte(Target) := Byte(Source);
    otSWord, otUWord:
      Word(Target) := Word(Source);
    otSLong, otULong:
      Longword(Target) := Longword(Source);
  end;
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
    otSLong, otULong:
      Result := LongInt(Value);
    else
      Result := -1;
  end;
end;

//=== TJvCustomAppStoreOptions ===============================================

constructor TJvCustomAppStoreOptions.Create;
begin
  inherited Create;
  BooleanStringTrueValues := 'TRUE, YES, Y';
  BooleanStringFalseValues := 'FALSE, NO, N';
  BooleanAsString := True;
  EnumerationAsString := True;
  TypedIntegerAsString := True;
  SetAsString := False;
  DateTimeAsString := True;
  DefaultIfReadConvertError := False;
  DefaultIfValueNotExists := True;
end;

function TJvCustomAppStoreOptions.IsValueListString(AValue, AList: string): Boolean;
begin
  with TStringList.Create do
    try
      CommaText := UpperCase(AList);
      Result := IndexOf(UpperCase(AValue)) >= 0;
    finally
      Free;
    end;
end;

function TJvCustomAppStoreOptions.DefaultTrueString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringTrueValues);
  if I = 0 then
    I := Length(FBooleanStringTrueValues) + 1;
  Result := Trim(Copy(FBooleanStringTrueValues, 1, I - 1));
end;

function TJvCustomAppStoreOptions.DefaultFalseString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringFalseValues);
  if I = 0 then
    I := Length(FBooleanStringFalseValues) + 1;
  Result := Trim(Copy(FBooleanStringFalseValues, 1, I - 1));
end;

function TJvCustomAppStoreOptions.IsValueTrueString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringTrueValues);
end;

function TJvCustomAppStoreOptions.IsValueFalseString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringFalseValues);
end;

procedure TJvCustomAppStoreOptions.SetBooleanAsString(Value: Boolean);
begin
  FBooleanAsString := Value and (DefaultTrueString <> '') and (DefaultFalseString <> '');
end;

procedure TJvCustomAppStoreOptions.SetBooleanStringTrueValues(Value: string);
begin
  FBooleanStringTrueValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultTrueString <> '')
end;

procedure TJvCustomAppStoreOptions.SetBooleanStringFalseValues(Value: string);
begin
  FBooleanStringFalseValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultFalseString <> '')
end;

procedure TJvCustomAppStoreOptions.SetEnumAsStr(Value: Boolean);
begin
  FEnumAsStr := Value;
end;

procedure TJvCustomAppStoreOptions.SetIntAsStr(Value: Boolean);
begin
  FIntAsStr := Value;
end;

procedure TJvCustomAppStoreOptions.SetSetAsStr(Value: Boolean);
begin
  FSetAsStr := Value;
end;

procedure TJvCustomAppStoreOptions.SetDateTimeAsStr(Value: Boolean);
begin
  FDateTimeAsString := Value;
end;

procedure TJvCustomAppStoreOptions.SetFloatAsStr(Value: Boolean);
begin
  FFloatAsString := Value;
end;

procedure TJvCustomAppStoreOptions.SetDefaultIfReadConvertError(Value: Boolean);
begin
  FDefaultIfReadConvertError := Value;
end;

procedure TJvCustomAppStoreOptions.SetDefaultIfValueNotExists(Value: Boolean);
begin
  FDefaultIfValueNotExists := Value;
end;

//=== TJvCustomAppStore ======================================================

constructor TJvCustomAppStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoreOptions := GetStoreOptionsClass.Create;
  FSubStores := TJvAppSubStores.Create(Self);
end;

destructor TJvCustomAppStore.Destroy;
begin
  FreeAndNil(FSubStores);
  FreeAndNil(FStoreOptions);
  inherited Destroy;
end;

procedure TJvCustomAppStore.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent is TJvCustomAppStore) and (Operation = opRemove) and
    Assigned(SubStores) then
    SubStores.Delete(AComponent as TJvCustomAppStore);
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

class function TJvCustomAppStore.GetStoreOptionsClass: TJvAppStoreOptionsClass;
begin
  Result := TJvAppStoreOptions;
end;

procedure TJvCustomAppStore.SplitKeyPath(const Path: string; out Key, ValueName: string);
var
  AbsPath: string;
  ValueNamePos: Integer;
begin
  AbsPath := GetAbsPath(Path);
  ValueNamePos := LastDelimiter('\', AbsPath);
  Key := StrLeft(AbsPath, ValueNamePos - 1);
  ValueName := StrRestOf(AbsPath, ValueNamePos + 1);
end;

procedure TJvCustomAppStore.SetSubStores(Value: TJvAppSubStores);
begin
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
  Sender.FStoreSL.Add(Sender.ReadString(Path + '\' + cItem + IntToStr(Index)));
end;

procedure TJvCustomAppStore.WriteSLItem(Sender: TJvCustomAppStore;
  const Path: string; const Index: Integer);
begin
  Sender.WriteString(Path + '\' + cItem + IntToStr(Index), Sender.FStoreSL[Index]);
end;

procedure TJvCustomAppStore.DeleteSLItems(Sender: TJvCustomAppStore;
  const Path: string; const First, Last: Integer);
var
  I: Integer;
begin
  for I := First to Last do
    Sender.DeleteValue(Path + '\' + cItem + IntToStr(I));
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
    if aeoValues in Options then
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
        if aeoRecursive in Options then
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

procedure TJvCustomAppStore.SetStoreOptions(Value: TJvCustomAppStoreOptions);
begin
  if (Value <> nil) and (Value <> FStoreOptions) then
    FStoreOptions.Assign(Value);
end;

procedure TJvCustomAppStore.DoTranslatePropertyName(Instance: TPersistent; var Name: string;
  const Reading: Boolean);
begin
  if Assigned(FOnTranslatePropertyName) then
    FOnTranslatePropertyName(Self, Instance, Name, Reading);
end;

function TJvCustomAppStore.HasSubStore(AStore: TJvCustomAppStore): Boolean;
var
  I: Integer;
begin
  I := SubStores.Count - 1;
  Result := False;
  while not Result and (I >= 0) do
  begin
    Result := (SubStores[I].AppStore = AStore) or
      ((SubStores[I].AppStore <> nil) and SubStores[I].AppStore.HasSubStore(AStore));
    Dec(I);
  end;
end;

function TJvCustomAppStore.ListStoredInt(const Path: string): Boolean;
begin
  Result := ValueStoredInt(StrEnsureSuffix('\', Path) + cCount);
end;

function TJvCustomAppStore.DoReadDateTime(const Path: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadFloat(Path, Default);
end;

procedure TJvCustomAppStore.DoWriteDateTime(const Path: string; Value: TDateTime);
begin
  DoWriteFloat(Path, Value);
end;

function TJvCustomAppStore.DoReadBoolean(const Path: string; Default: Boolean): Boolean;
begin
  Result := DoReadInteger(Path, Ord(Default)) <> Ord(False);
end;

procedure TJvCustomAppStore.DoWriteBoolean(const Path: string; Value: Boolean);
begin
  DoWriteInteger(Path, Ord(Value));
end;

function TJvCustomAppStore.ReadIntegerInt(const Path: string; Default: Integer): Integer;
begin
  if not ValueStoredInt(Path) and StoreOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      Result := DoReadInteger(Path, Default);
    except
      on E: EConvertError do
        if StoreOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStore.WriteIntegerInt(const Path: string; Value: Integer);
begin
  DoWriteInteger(Path, Value);
end;

function TJvCustomAppStore.ReadFloatInt(const Path: string; Default: Extended): Extended;
begin
  if not ValueStoredInt(Path) and StoreOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      if StoreOptions.FloatAsString then
        try
          Result := StrToFloat(DoReadString(Path, FloatToStr(Default)));
        except
          on E: EConvertError do
            Result := DoReadFloat(Path, Default);
        end
      else
        try
          Result := DoReadFloat(Path, Default);
        except
          on E: EConvertError do
            Result := StrToFloat(DoReadString(Path, FloatToStr(Default)));
        end
    except
      on E: EConvertError do
        if StoreOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStore.WriteFloatInt(const Path: string; Value: Extended);
begin
  if StoreOptions.FloatAsString then
    DoWriteString(Path, FloatToStr(Value))
  else
    DoWriteFloat(Path, Value);
end;

function TJvCustomAppStore.ReadStringInt(const Path: string; Default: string): string;
begin
  if not ValueStoredInt(Path) and StoreOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      Result := DoReadString(Path, Default);
    except
      on E: EConvertError do
        if StoreOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStore.WriteStringInt(const Path: string; Value: string);
begin
  DoWriteString(Path, Value);
end;

function TJvCustomAppStore.ReadBinaryInt(const Path: string; var Buf; BufSize: Integer): Integer;
begin
  Result := DoReadBinary(Path, Buf, BufSize);
end;

procedure TJvCustomAppStore.WriteBinaryInt(const Path: string; const Buf; BufSize: Integer);
begin
  DoWriteBinary(Path, Buf, BufSize);
end;

function TJvCustomAppStore.ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime;
begin
  if not ValueStoredInt(Path) and StoreOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      if StoreOptions.DateTimeAsString then
        try
          Result := StrToDateTime(DoReadString(Path, DateTimeToStr(Default)));
        except
          on E: EConvertError do
            Result := DoReadDateTime(Path, Default);
        end
      else
        try
          Result := DoReadDateTime(Path, Default);
        except
          on E: EConvertError do
            Result := StrToDateTime(DoReadString(Path, DateTimeToStr(Default)));
        end
    except
      on E: EConvertError do
        if StoreOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStore.WriteDateTimeInt(const Path: string; Value: TDateTime);
begin
  if StoreOptions.DateTimeAsString then
    DoWriteString(Path, DateTimeToStr(Value))
  else
    DoWriteFloat(Path, Value);
end;

function TJvCustomAppStore.ReadBooleanInt(const Path: string; Default: Boolean): Boolean;
var
  Value: string;
begin
  if not ValueStoredInt(Path) and StoreOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      try
        if Default then
          Value := DoReadString(Path, StoreOptions.DefaultTrueString)
        else
          Value := DoReadString(Path, StoreOptions.DefaultFalseString);
        if StoreOptions.IsValueTrueString(Value) then
          Result := True
        else
        if StoreOptions.IsValueFalseString(Value) then
          Result := False
        else
          Result := DoReadBoolean(Path, Default);
      except
        on E: EConvertError do
          Result := DoReadBoolean(Path, Default);
      end;
    except
      on E: EConvertError do
        if StoreOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStore.WriteBooleanInt(const Path: string; Value: Boolean);
begin
  if StoreOptions.BooleanAsString then
    if Value then
      DoWriteString(Path, StoreOptions.DefaultTrueString)
    else
      DoWriteString(Path, StoreOptions.DefaultFalseString)
  else
    DoWriteBoolean(Path, Value);
end;

class function TJvCustomAppStore.NameIsListItem(Name: string): Boolean;
var
  NameStart: PChar;
begin
  NameStart := AnsiStrRScan(pchar(Name), '\');
  if NameStart = nil then
    NameStart := PChar(Name);
  Result := (AnsiStrLIComp(NameStart, cItem, 4) = 0) and (NameStart[4] in ['0' .. '9']);
end;

class function TJvCustomAppStore.ConcatPaths(const Paths: array of string): string;
begin
  Result := OptimizePaths(Paths);
end;

procedure TJvCustomAppStore.ResolvePath(InPath: string; out TgtStore: TJvCustomAppStore;
  out TgtPath: string);
var
  SubStoreItem: TJvAppSubStore;
begin
  TgtPath := '\' + ConcatPaths([Path, InPath]);
  TgtStore := Self;
  SubStoreItem := SubStores.MatchFor(TgtPath);
  if (SubStoreItem <> nil) and (SubStoreItem.AppStore <> nil) then
  begin
    TgtStore := SubStoreItem.AppStore;
    Delete(TgtPath, 1, Length(SubStoreItem.RootPath) + 1);
    TgtPath := '\' + OptimizePaths([TgtPath]);
    if TgtPath = '\' then
      raise EJVCLAppStoreError.Create(SInvalidPath);
  end;
end;

function TJvCustomAppStore.IsFolder(Path: string; ListIsValue: Boolean): Boolean;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.IsFolderInt(TgtPath, ListIsValue);
end;

function TJvCustomAppStore.PathExists(const Path: string): Boolean;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.PathExistsInt(TgtPath);
end;

function TJvCustomAppStore.ValueStored(const Path: string): Boolean;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ValueStoredInt(TgtPath);
end;

function TJvCustomAppStore.ListStored(const Path: string): Boolean;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ListStoredInt(TgtPath);
end;

procedure TJvCustomAppStore.DeleteValue(const Path: string);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.DeleteValueInt(TgtPath);
end;

procedure TJvCustomAppStore.DeleteSubTree(const Path: string);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.DeleteSubTreeInt(Path);
end;

function TJvCustomAppStore.ReadInteger(const Path: string; Default: Integer): Integer;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ReadIntegerInt(TgtPath, Default);
end;

procedure TJvCustomAppStore.WriteInteger(const Path: string; Value: Integer);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteIntegerInt(TgtPath, Value);
end;

function TJvCustomAppStore.ReadFloat(const Path: string; Default: Extended): Extended;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ReadFloatInt(TgtPath, Default);
end;

procedure TJvCustomAppStore.WriteFloat(const Path: string; Value: Extended);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteFloatInt(TgtPath, Value);
end;

function TJvCustomAppStore.ReadString(const Path: string; Default: string): string;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ReadStringInt(TgtPath, Default);
end;

procedure TJvCustomAppStore.WriteString(const Path: string; Value: string);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteStringInt(TgtPath, Value);
end;

function TJvCustomAppStore.ReadBinary(const Path: string; var Buf; BufSize: Integer): Integer;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ReadBinaryInt(TgtPath, Buf, BufSize);
end;

procedure TJvCustomAppStore.WriteBinary(const Path: string; const Buf; BufSize: Integer);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteBinaryInt(TgtPath, Buf, BufSize);
end;

function TJvCustomAppStore.ReadDateTime(const Path: string; Default: TDateTime): TDateTime;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ReadDateTimeInt(TgtPath, Default);
end;

procedure TJvCustomAppStore.WriteDateTime(const Path: string; Value: TDateTime);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteDateTimeInt(TgtPath, Value);
end;

function TJvCustomAppStore.ReadBoolean(const Path: string; Default: Boolean): Boolean;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  Result := TgtStore.ReadBooleanInt(TgtPath, Default);
end;

procedure TJvCustomAppStore.WriteBoolean(const Path: string; Value: Boolean);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteBooleanInt(TgtPath, Value);
end;

function TJvCustomAppStore.ReadList(const Path: string;
  const OnReadItem: TAppStoreListItemEvent): Integer;
var
  I: Integer;
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path + '\*', TgtStore, TgtPath);
  Delete(TgtPath, Length(TgtPath) - 1, 2);
  Result := TgtStore.ReadIntegerInt(TgtPath + '\' + cCount, 0);
  for I := 0 to Result - 1 do
    OnReadItem(TgtStore, TgtPath, I);
end;

procedure TJvCustomAppStore.WriteList(const Path: string; const ItemCount: Integer;
  const OnWriteItem: TAppStoreListItemEvent; const OnDeleteItems: TAppStoreListDeleteEvent);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
  PrevListCount: Integer;
  I: Integer;
begin
  ResolvePath(Path + '\*', TgtStore, TgtPath);
  Delete(TgtPath, Length(TgtPath) - 1, 2);
  PrevListCount := TgtStore.ReadIntegerInt(TgtPath + '\' + cCount, 0);
  TgtStore.WriteIntegerInt(TgtPath + '\' + cCount, ItemCount);
  for I := 0 to ItemCount - 1 do
    OnWriteItem(TgtStore, TgtPath, I);
  if (PrevListCount > ItemCount) and Assigned(OnDeleteItems) then
    OnDeleteItems(TgtStore, TgtPath, ItemCount, PrevListCount - 1);
end;

function TJvCustomAppStore.ReadStringList(const Path: string; const SL: TStrings;
  const ClearFirst: Boolean): Integer;
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path + '\*', TgtStore, TgtPath);
  Delete(TgtPath, Length(TgtPath) - 1, 2);
  if ClearFirst then
    SL.Clear;
  TgtStore.FStoreSL := SL;
  Result := TgtStore.ReadList(TgtPath, TgtStore.ReadSLItem);
end;

procedure TJvCustomAppStore.WriteStringList(const Path: string; const SL: TStrings);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path + '\*', TgtStore, TgtPath);
  Delete(TgtPath, Length(TgtPath) - 1, 2);
  TgtStore.FStoreSL := SL;
  TgtStore.WriteList(TgtPath, SL.Count, TgtStore.WriteSLItem, TgtStore.DeleteSLItems);
end;

procedure TJvCustomAppStore.ReadEnumerationInt(const Path: string; const TypeInfo: PTypeInfo;
  const Default; out Value);
var
  OrdValue: Integer;
  Conv: TIdentToInt;
  S: string;
  TmpDefReadError: Boolean;
begin
  if not ValueStoredInt(Path) and StoreOptions.DefaultIfValueNotExists then
    CopyEnumValue(Default, Value, GetTypeData(TypeInfo).OrdType)
  else
  begin
    OrdValue := 0;
    CopyEnumValue(Default, OrdValue, GetTypeData(TypeInfo).OrdType);
    if (TypeInfo = System.TypeInfo(Boolean)) or ((TypeInfo.Kind = tkEnumeration) and
        (GetTypeData(GetTypeData(TypeInfo).BaseType^).MinValue < 0)) then
      OrdValue := Ord(ReadBooleanInt(Path, OrdValue <> 0))
    else
    begin
      try
        if TypeInfo.Kind = tkInteger then
        begin
          { Could be stored as a normal int or as an identifier. Try identifier first as that will
            not raise an exception }
          Conv := FindIdentToInt(TypeInfo);
          if Assigned(Conv) then
          begin
            TmpDefReadError := StoreOptions.DefaultIfReadConvertError;
            StoreOptions.DefaultIfReadConvertError := True;
            try
              S := ReadStringInt(Path, '');
            finally
              StoreOptions.DefaultIfReadConvertError := TmpDefReadError;
            end;
            if (S = '') or not (Conv(S, OrdValue)) then
              OrdValue := ReadIntegerInt(Path, OrdValue);
          end
          else
            OrdValue := ReadIntegerInt(Path, OrdValue);
        end
        else if TypeInfo.Kind = tkEnumeration then
        begin
          // Usage of an invalid identifier to signal the value does not exist
          OrdValue := GetEnumValue(TypeInfo, ReadStringInt(Path, cInvalidIdentifier));
          if OrdValue = -1 then
            OrdValue := ReadIntegerInt(Path, OrdValue);
        end
        else
          raise EJVCLAppStoreError.Create(SInvalidType);
      except
        on E: EConvertError do
          if StoreOptions.DefaultIfReadConvertError then
            CopyEnumValue(Default, OrdValue, GetTypeData(TypeInfo).OrdType)
          else
            raise;
      end;
    end;
    CopyEnumValue(OrdValue, Value, GetTypeData(TypeInfo).OrdType);
  end;
end;

procedure TJvCustomAppStore.WriteEnumerationInt(const Path: string; const TypeInfo: PTypeInfo;
  const Value);
var
  Conv: TIntToIdent;
  S: string;
begin
  if TypeInfo = System.TypeInfo(Boolean) then
    WriteBooleanInt(Path, Boolean(Value))
  else
  if (TypeInfo.Kind = tkEnumeration) and
      (GetTypeData(GetTypeData(TypeInfo).BaseType^).MinValue < 0) then
    WriteBooleanInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType) <> 0)
  else
  if TypeInfo.Kind = tkInteger then
  begin
    if StoreOptions.TypedIntegerAsString then
    begin
      Conv := FindIntToIdent(TypeInfo);
      if Assigned(Conv) and Conv(OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType), S) then
        WriteStringInt(Path, S)
      else
        WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
    end
    else
      WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
  end
  else
  if TypeInfo.Kind = tkEnumeration then
  begin
    if StoreOptions.EnumerationAsString then
      WriteStringInt(Path, GetEnumName(TypeInfo, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType)))
    else
      WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
  end
  else
    raise EJVCLAppStoreError.Create(SInvalidType);
end;

procedure TJvCustomAppStore.ReadEnumeration(const Path: string; const TypeInfo: PTypeInfo;
  const Default; out Value);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.ReadEnumerationInt(TgtPath, TypeInfo, Default, Value);
end;

procedure TJvCustomAppStore.WriteEnumeration(const Path: string; const TypeInfo: PTypeInfo;
  const Value);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteEnumerationInt(TgtPath, TypeInfo, Value);
end;

procedure TJvCustomAppStore.ReadSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Default;
  out Value);
var
  Lst: TStrings;
  I: Integer;
begin
  if IsFolder(Path) then
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
                  if ReadBooleanInt(ConcatPaths([Path, GetNames(I)]), False) then
                    Lst.Add(GetNames(I));
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).SetAsList(Value, Lst);
            end;
          tkChar:
            begin
              JclStrToSet(ATypeInfo, Value, ''); // empty out value
              for I := GetMinValue to GetMaxValue do
                if ReadBooleanInt(ConcatPaths([Path, GetCharName(Chr(I))]), False) then
                  Include(TIntegerSet(Value), I);
            end;
          tkInteger:
            begin
              for I := GetMinValue to GetMaxValue do
                if ReadBooleanInt(ConcatPaths([Path, GetIntName(I)]), False) then
                  Lst.Add(IntToStr(I));
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).SetAsList(Value, Lst);
            end;
          else
            raise EJVCLAppStoreError.Create(SUnknownBaseType);
        end;
      end;
    finally
      FreeAndNil(Lst);
    end;
  end
  else // It's stored as a string value or not stored at all
    JclStrToSet(ATypeInfo, Value, ReadStringInt(Path, JclSetToStr(ATypeInfo, Default, True)));
end;

procedure TJvCustomAppStore.WriteSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Value);
var
  Lst: TStrings;
  I: Integer;
begin
  if StoreOptions.SetAsString then
    WriteStringInt(Path, JclSetToStr(ATypeInfo, Value, True))
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
                  WriteBooleanInt(ConcatPaths([Path, GetNames(I)]),
                    Lst.IndexOf(GetNames(I)) > - 1);
            end;
          tkChar:
            begin
              for I := GetMinValue to GetMaxValue do
                WriteBooleanInt(ConcatPaths([Path, GetCharName(Chr(I))]), I in TIntegerSet(Value));
            end;
          tkInteger:
            begin
              (JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).GetAsList(Value, False, Lst);
              for I := GetMinValue to GetMaxValue do
                WriteBooleanInt(ConcatPaths([Path, GetIntName(I)]),
                  Lst.IndexOf(IntToStr(I)) > - 1);
            end;
          else
            raise EJVCLAppStoreError.Create(SUnknownBaseType);
        end;
      end;
    finally
      FreeAndNil(Lst);
    end;
  end;
end;

procedure TJvCustomAppStore.ReadSet(const Path: string; const ATypeInfo: PTypeInfo; const Default;
  out Value);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.ReadSetInt(TgtPath, ATypeInfo, Default, Value);
end;

procedure TJvCustomAppStore.WriteSet(const Path: string; const ATypeInfo: PTypeInfo; const Value);
var
  TgtStore: TJvCustomAppStore;
  TgtPath: string;
begin
  ResolvePath(Path, TgtStore, TgtPath);
  TgtStore.WriteSetInt(TgtPath, ATypeInfo, Value);
end;

procedure TJvCustomAppStore.ReadPersistent(const Path: string; const PersObj: TPersistent;
  const Recursive, ClearFirst: Boolean);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  TmpValue: Integer;
  SubObj: TObject;
begin
  if not Assigned(PersObj) then
    Exit;
  for Index := 0 to GetPropCount(PersObj) - 1 do
  begin
    PropName := GetPropName(PersObj, Index);
    KeyName := TranslatePropertyName(PersObj, PropName, True);
    PropPath := ConcatPaths([Path, KeyName]);
    case PropType(PersObj, PropName) of
      tkLString, tkWString, tkString:
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
      tkChar, tkInteger:
        begin
          TmpValue := GetOrdProp(PersObj, PropName);
          ReadEnumeration(PropPath, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
          SetOrdProp(PersObj, PropName, TmpValue);
        end;
      tkInt64:
        SetInt64Prop(PersObj, PropName, StrToInt64(ReadString(PropPath,
          IntToStr(GetInt64Prop(PersObj, PropName)))));
      tkFloat:
        SetFloatProp(PersObj, PropName, ReadFloat(PropPath, GetFloatProp(PersObj, PropName)));
      tkClass:
        begin
          SubObj := GetObjectProp(PersObj, PropName);
          if SubObj is TStrings then
            ReadStringList(PropPath, TStrings(SubObj), ClearFirst)
          else
          if (SubObj is TPersistent) and Recursive then
            if SubObj is TJvCustomPropertyStore then
              TJvCustomPropertyStore(SubObj).LoadProperties
            else
              ReadPersistent(PropPath, TPersistent(SubObj), True, ClearFirst);
        end;
    end;
  end;
end;

procedure TJvCustomAppStore.WritePersistent(const Path: string; const PersObj: TPersistent;
  const Recursive: Boolean; const IgnoreProperties: TStrings);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  TmpValue: Integer;
  SubObj: TObject;
begin
  if not Assigned(PersObj) then
    Exit;
  for Index := 0 to GetPropCount(PersObj) - 1 do
  begin
    PropName := GetPropName(PersObj, Index);
    KeyName := TranslatePropertyName(PersObj, PropName, False);
    PropPath := ConcatPaths([Path, KeyName]);
    if (IgnoreProperties = nil) or (IgnoreProperties.IndexOf(PropName) = -1) then
      case PropType(PersObj, PropName) of
        tkLString, tkWString, tkString:
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
        tkChar, tkInteger:
          begin
            if StoreOptions.TypedIntegerAsString then
            begin
              TmpValue := GetOrdProp(PersObj, PropName);
              WriteEnumeration(PropPath, GetPropInfo(PersObj, PropName).PropType^, TmpValue);
            end
            else
              WriteInteger(PropPath, GetOrdProp(PersObj, PropName));
          end;
        tkInt64:
          WriteString(PropPath, IntToStr(GetInt64Prop(PersObj, PropName)));
        tkFloat:
          WriteFloat(PropPath, GetFloatProp(PersObj, PropName));
        tkClass:
          begin
            SubObj := GetObjectProp(PersObj, PropName);
            if SubObj is TStrings then
              WriteStringList(PropPath, TStrings(SubObj))
            else
            if (SubObj is TPersistent) and Recursive then
              if SubObj is TJvCustomPropertyStore then
                TJvCustomPropertyStore(SubObj).StoreProperties
              else
                WritePersistent(PropPath, TPersistent(SubObj), True, IgnoreProperties);
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

function TJvCustomAppStore.TranslatePropertyName(Instance: TPersistent; const AName: string;
  const Reading: Boolean): string;
begin
  Result := AName;
  DoTranslatePropertyName(Instance, Result, Reading);
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
        '\', SearchPath, Strings, Options);
    I := Strings.IndexOf(OptimizePaths([Self.Path, SearchPath]));
    if I > -1 then
      Strings.Delete(I);
  finally
    Strings.EndUpdate;
  end;
end;

//=== TJvAppStore ============================================================

function TJvAppStore.IsFolderInt(Path: string; ListIsValue: Boolean): Boolean;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.PathExistsInt(const Path: string): Boolean;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.ValueStoredInt(const Path: string): Boolean;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.DeleteValueInt(const Path: string);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.DeleteSubTreeInt(const Path: string);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.ReadIntegerInt(const Path: string; Default: Integer): Integer;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteIntegerInt(const Path: string; Value: Integer);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.ReadFloatInt(const Path: string; Default: Extended): Extended;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteFloatInt(const Path: string; Value: Extended);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.ReadStringInt(const Path: string; Default: string): string;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteStringInt(const Path: string; Value: string);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.ReadBinaryInt(const Path: string; var Buf; BufSize: Integer): Integer;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteBinaryInt(const Path: string; const Buf; BufSize: Integer);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteDateTimeInt(const Path: string; Value: TDateTime);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

function TJvAppStore.ReadBooleanInt(const Path: string; Default: Boolean): Boolean;
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteBooleanInt(const Path: string; Value: Boolean);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.ReadEnumerationInt(const Path: string; const TypeInfo: PTypeInfo; const Default; out Value);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteEnumerationInt(const Path: string; const TypeInfo: PTypeInfo; const Value);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.ReadSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Default; out Value);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

procedure TJvAppStore.WriteSetInt(const Path: string; const ATypeInfo: PTypeInfo; const Value);
begin
  raise EJVCLAppStoreError.Create(SInvalidPath);
end;

//=== TJvAppSubStores ========================================================

constructor TJvAppSubStores.Create(AOwner: TJvCustomAppStore);
begin
  inherited Create(AOwner, TJvAppSubStore);
end;

function TJvAppSubStores.GetRootStorage: TJvCustomAppStore;
begin
  Result := TJvCustomAppStore(GetOwner);
end;

function TJvAppSubStores.GetItem(I: Integer): TJvAppSubStore;
begin
  Result := TJvAppSubStore(inherited GetItem(I));
end;

procedure TJvAppSubStores.SetItem(I: Integer; Value: TJvAppSubStore);
begin
  inherited SetItem(I, Value);
end;

procedure TJvAppSubStores.RootOptionsChanged;
begin
end;

function TJvAppSubStores.CheckUniqueBase(APath: string; IgnoreIndex: Integer): Boolean;
begin
  Result := MatchFor(OptimizePaths([APath]) + '\*', IgnoreIndex) = nil;
end;

function TJvAppSubStores.MatchFor(APath: string; IgnoreIndex: Integer): TJvAppSubStore;
var
  I: Integer;
begin
  Result := nil;
  APath := OptimizePaths([APath]);
  // APath is now a valid path, stripped from it's leading/trailing backslashes
  for I := 0 to Count - 1 do
    if I <> IgnoreIndex then
      if StrLIComp(PChar(Items[I].RootPath), PChar(APath), Length(Items[I].RootPath)) = 0 then
        // Possible match. Check if next char is a \
        if APath[Length(Items[I].RootPath) + 1] = '\' then
          { Next char in APath is a backslash, so we have a valid match. Check with any previous
            to see if it is better than that one. }
          if (Result = nil) or (Length(Result.RootPath) < Length(Items[I].RootPath)) then
            Result := Items[I]; // no previous match or new match is close to what we searched for
end;

procedure TJvAppSubStores.Add(RootPath: string; AppStore: TJvCustomAppStore);
var
  Tmp: TJvAppSubStore;
begin
  Tmp := TJvAppSubStore.Create(Self);
  try
    Tmp.RootPath := RootPath;
    Tmp.AppStore := AppStore;
  // (rom) Tmp is orphaned if no exception hits
  except
    FreeAndNil(Tmp);
    raise;
  end;
end;

procedure TJvAppSubStores.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TJvAppSubStores.Delete(RootPath: string; const IncludeSubPaths: Boolean);
var
  I: Integer;
  SubPath: string;
  CmpLen: Integer;
begin
  RootPath := OptimizePaths([RootPath]);
  if RootPath <> '' then
  begin
    SubPath := RootPath + '\';
    CmpLen := Length(SubPath);
    I := Count - 1;
    while I >= 0 do
    begin
      if AnsiSameText(RootPath, Items[I].RootPath) or (IncludeSubPaths and
          (StrLIComp(PChar(SubPath), PChar(Items[I].RootPath), CmpLen) = 0)) then
        Delete(I);
      Dec(I);
    end;
  end;
end;

procedure TJvAppSubStores.Delete(AppStore: TJvCustomAppStore);
var
  I: Integer;
begin
  I := Count - 1;
  while (I >= 0) do
  begin
    if Items[I].AppStore  = AppStore then
      Delete(I);
    Dec(I);
  end;
end;

//=== TJvAppSubStore =========================================================

function TJvAppSubStore.GetOwnerStore: TJvCustomAppStore;
begin
  Result := TJvAppSubStores(Collection).RootStorage;
end;

function TJvAppSubStore.GetDisplayName: string;
begin
  if (RootPath <> '') and (AppStore <> nil) then
    Result := '\' + RootPath + '=' + AppStore.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TJvAppSubStore.SetRootPath(Value: string);
begin
  Value := OptimizePaths([Value]);
  if Value <> RootPath then
    if TJvAppSubStores(Collection).CheckUniqueBase(Value, Index) then
      FRootPath := Value
    else
      raise EJVCLAppStoreError.CreateFmt(SNotAUniqueRootPath, [Value]);
end;

procedure TJvAppSubStore.SetAppStore(Value: TJvCustomAppStore);
begin
  if Value <> AppStore then
  begin
    if (Value <> nil) and (Value.HasSubStore(OwnerStore) or (Value = OwnerStore)) then
      raise EJVCLAppStoreError.Create(SCircularReferenceOfStorages);
    if AppStore <> nil then
      AppStore.RemoveFreeNotification(OwnerStore);
    FAppStore := Value;
    if AppStore <> nil then
      AppStore.FreeNotification(OwnerStore);
  end;
end;

end.
