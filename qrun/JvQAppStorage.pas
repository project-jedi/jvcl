{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppStorage.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jens Fudickar
  Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
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
  to as 'asUserIniBackend' from now on) using asRegBackend.SubStorages. The RootPath for the
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
  and you are ready to read in the settings of that user.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQAppStorage;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, TypInfo,
  {$IFDEF UNIX}
  JvQJCLUtils,
  {$ENDIF UNIX}
  JvQComponent, JvQTypes;

const
  // (rom) this name is shared in several units and should be made global
  cItem = 'Item';

type
  TJvCustomAppStorage = class;
  TJvAppStorage = class;
  TJvCustomAppStorageOptions = class;
  TJvAppSubStorages = class;
  TJvAppSubStorage = class;

  EJVCLAppStorageError = class(EJVCLException);

  { TAppStorage does not automatically store published properties of a class that
    supports the IJvAppStorageHandler interface. Instead it invokes the Read and
    Write methods. }
  IJvAppStorageHandler = interface
  ['{E3754817-49A3-4612-A228-5D44A088681D}']
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  end;

  { TAppStorage automatically stores published properties of a class that
    supports the IJvAppStoragePublishedProps interface, even if the class
    supports the IJvAppStorageHandler interface, too. }
  IJvAppStoragePublishedProps = interface
  ['{0211AEF7-CCE9-4F13-B3CE-287251C89182}']
  end;

  TJvAppStorageListItemEvent = procedure(Sender: TJvCustomAppStorage; const Path: string;
    const List: TObject; const Index: Integer; const ItemName: string) of object;
  TJvAppStorageListDeleteEvent = procedure(Sender: TJvCustomAppStorage; const Path: string;
    const List: TObject; const First, Last: Integer; const ItemName: string) of object;
  TJvAppStoragePropTranslateEvent = procedure(Sender: TJvCustomAppStorage; Instance: TPersistent;
    var Name: string; const Reading: Boolean) of object;
  TJvAppStorageCryptEvent = procedure(var Value: string) of object;
  TJvAppStorageGetFileNameEvent = procedure(Sender: TJvCustomAppStorage;
    var FileName: TFileName) of object;
  TJvAppStorageObjectListItemCreateEvent = function(Sender: TJvCustomAppStorage; const Path: string; Index: Integer): TPersistent of object;

  TJvAppStorageOptionsClass = class of TJvCustomAppStorageOptions;

  TJvAppStorageEnumOption = (
    aeoFolders,           // report folders
    aeoValues,            // report values
    aeoReportListAsValue, // report list as value (a list is actually a folder containing a Count and Item? values)
    aeoReportRelative,    // report all found folders and values relative to the requested path (otherwise relative to the Root path)
    aeoRecursive);        // scan sub folders as well
  TJvAppStorageEnumOptions = set of TJvAppStorageEnumOption;

  TFileLocation = (
    flCustom,       // FileName property will contain full path
    {$IFDEF MSWINDOWS}
    flWindows,      // Store in %WINDOWS%; only use file name part of FileName property.
    {$ENDIF MSWINDOWS}
    flTemp,         // Store in %TEMP%; only use file name part of FileName property.
    flExeFile,      // Store in same folder as application's exe file; only use file name part of FileName property.
    flUserFolder);  // Store in %USER%\Application Data. Use the FileName property if it's a relative path or only the file name part of FileName property.

  TJvCustomAppStorage = class(TJvComponent)
  private
    FRoot: string;
    FCurPath: string;
    FStorageOptions: TJvCustomAppStorageOptions;
    FSubStorages: TJvAppSubStorages;
    FOnTranslatePropertyName: TJvAppStoragePropTranslateEvent;
    FOnEncryptPropertyValue: TJvAppStorageCryptEvent;
    FOnDecryptPropertyValue: TJvAppStorageCryptEvent;
    FCryptEnabledStatus: Integer;
    FAutoFlush: Boolean;
    FUpdateCount: Integer;
    FAutoReload: Boolean;
    FCurrentInstanceCreateEvent: TJvAppStorageObjectListItemCreateEvent;
    FReadOnly: Boolean;
    function GetUpdating: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;


    //Returns the property count of an instance
    function GetPropCount(Instance: TPersistent): Integer;
    //Returns the property name of an instance at a certain index
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetStorageOptionsClass: TJvAppStorageOptionsClass; virtual;
    { Split the specified path into an absolute path and a value name (the last item in the path
      string). Just a helper for all the storage methods. }
    procedure SplitKeyPath(const Path: string; out Key, ValueName: string); virtual;
    { SubStorages property set method. Does nothing. }
    procedure SetSubStorages(Value: TJvAppSubStorages);
    { Retrieve application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    function GetRoot: string;
    { Set application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    procedure SetRoot(const Value: string);
    { Retrieves currently set path (including the Root path). }
    function GetCurrentPath: string;
    { Returns the path as an absolute path (including the Root path). If the given path does not
      start with a backslash (\) the path is appended to the Root path, resolving any references to
      parent folders. }
    function GetAbsPath(const Path: string): string;
    { StringList item reader used by ReadStringList in the call to ReadList. }
    procedure ReadStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item writer used by WriteStringList in the call to WriteList. }
    procedure WriteStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item deleter used by WriteStringList in the call to WriteList. }
    procedure DeleteStringListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);

    { Default Function for creating a new Object. The classname could be received from the AppStorage using the Path "Classname" }
    function DefaultObjectListItemCreateEvent(Sender: TJvCustomAppStorage; const Path: string; Index: Integer): TPersistent;

    { ObjectList item reader used by ReadObjectList in the call to ReadList. }
    procedure ReadObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { ObjectList item writer used by WriteObjectList in the call to WriteList. }
    procedure WriteObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { ObjectList item deleter used by WriteObjectList in the call to WriteList. }
    procedure DeleteObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);

    { StringList item reader used by ReadStringObjectList in the call to ReadList. }
    procedure ReadStringObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item writer used by WriteStringObjectList in the call to WriteList. }
    procedure WriteStringObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { StringList item deleter used by WriteStringObjectList in the call to WriteList. }
    procedure DeleteStringObjectListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);

    { Collection item reader used by ReadCollection in the call to ReadList. }
    procedure ReadCollectionItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { Collection item writer used by WriteCollection in the call to WriteList. }
    procedure WriteCollectionItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { Collection item deleter used by WriteCollection in the call to WriteList. }
    procedure DeleteCollectionItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);

    { Enum all folders in the specified folder. }
    procedure EnumFolders(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); virtual; abstract;
    { Enum all values below in the specified folder. }
    procedure EnumValues(const Path: string; const Strings: TStrings;
      const ReportListAsValue: Boolean = True); virtual; abstract;
    { Internal retrieval of GetStoredValues. Is used to handle recursiveness. }
    procedure InternalGetStoredValues(const PrefixPath, SearchPath: string;
      const Strings: TStrings; const Options: TJvAppStorageEnumOptions);
    { Current root path for storage. Paths used in other methods are relative to this path. }
    function GetPath: string;
    { Specify a new root. Given path is relative to the current path. Se remarks above }
    procedure SetPath(const Path: string);
    { Determines if the specified name belongs to a list value. }
    class function NameIsListItem(const Name: string): Boolean;
    { Application specific root. Path is prepended to any specified path and serves as an absolute
      root for any reading/writing. Not all implementation will use it. Generally it's used for
      storages not specific to an application (such as the registry). }
    property Root: string read GetRoot write SetRoot;
    { Set the StorageOptions Property }
    procedure SetStorageOptions(Value: TJvCustomAppStorageOptions);
    { Invokes the OnTranslatePropertyName event if one is assigned. }
    procedure DoTranslatePropertyName(Instance: TPersistent; var Name: string;
      const Reading: Boolean);
    { Determines if the specified is a sub store of this storage (will scan the entire sub storage
      hierarchy. }
    function HasSubStorage(AStore: TJvCustomAppStorage): Boolean;

    { Determines if the path represents a folder (ignores sub stores) }
    function IsFolderInt(const Path: string; ListIsValue: Boolean = True): Boolean; virtual; abstract;
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
    function DoReadString(const Path: string; const Default: string): string; virtual; abstract;
    { Stores an string value. }
    procedure DoWriteString(const Path: string; const Value: string); virtual; abstract;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function DoReadBinary(const Path: string; Buf: Pointer; BufSize: Integer): Integer; virtual; abstract;
    { Stores a buffer. }
    procedure DoWriteBinary(const Path: string; Buf: Pointer; BufSize: Integer); virtual; abstract;
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
    function ReadStringInt(const Path: string; const Default: string): string; virtual;
    { Stores an string value (ignores sub stores). }
    procedure WriteStringInt(const Path: string; const Value: string); virtual;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved (ignores sub stores). }
    function ReadBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer): Integer; virtual;
    { Stores a buffer (ignores sub stores). }
    procedure WriteBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer); virtual;
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
    procedure ReadEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Default;
      out Value); virtual;
    { Stores an enumeration (ignores sub stores). }
    procedure WriteEnumerationInt(const Path: string; TypeInfo: PTypeInfo;
      const Value); virtual;
    { Retrieves a set. If the value is not found, the Default will be returned (ignores sub
      stores). }
    procedure ReadSetInt(const Path: string; ATypeInfo: PTypeInfo; const Default;
      out Value); virtual;
    { Stores a set (ignores sub stores). }
    procedure WriteSetInt(const Path: string; ATypeInfo: PTypeInfo; const Value); virtual;

    function EncryptPropertyValue(Value: string): string;
    function DecryptPropertyValue(Value: string): string;

    procedure SetReadOnly (Value: Boolean);
    function GetReadOnly: Boolean;
    function GetPhysicalReadOnly : Boolean; virtual;

    property SubStorages: TJvAppSubStorages read FSubStorages write SetSubStorages;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // (p3) moved Flush, Reload and AutoFlush to the base storage because users
    // should be able to call Flush and Reload as needed without being dependant on whether
    // the spcific storage implements it or not. Also made them virtual - if Flush and Reload
    // doesn't make sense for a specific storage, it shouldn't have to implement them
    procedure Flush; virtual;
    procedure Reload; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    property IsUpdating: Boolean read GetUpdating;
    property AutoFlush: Boolean read FAutoFlush write FAutoFlush default False;
    property AutoReload: Boolean read FAutoReload write FAutoReload default False;
    class function ConcatPaths(const Paths: array of string): string;
    { Resolve a path to it's actual used storage backend and root path. }
    procedure ResolvePath(const InPath: string; out TargetStore: TJvCustomAppStorage; out TargetPath: string);
    { Determines if the path represents a folder }
    function IsFolder(const Path: string; ListIsValue: Boolean = True): Boolean;
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
    function ReadString(const Path: string; const Default: string = ''): string;
    { Stores an string value. }
    procedure WriteString(const Path: string; const Value: string);
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function ReadDateTime(const Path: string; Default: TDateTime = 0): TDateTime;
    { Stores a TDateTime value. }
    procedure WriteDateTime(const Path: string; Value: TDateTime);
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function ReadBinary(const Path: string; Buf: Pointer; BufSize: Integer): Integer;
    { Stores a buffer. }
    procedure WriteBinary(const Path: string; Buf: Pointer; BufSize: Integer);
    { Retrieves the specified list. Caller provides a callback method that will read the individual
      items. ReadList will first determine the number of items to read and calls the specified
      method for each item. }
    function ReadList(const Path: string;
      const List: TObject; const OnReadItem: TJvAppStorageListItemEvent;
      const ItemName: string = cItem): Integer;
    { Stores a list of items. The number of items is stored first. For each item the provided
      item write method is called. Any additional items in the list (from a previous write) will be
      removed by the optionally provided delete method. }
    procedure WriteList(const Path: string; const List: TObject; const ItemCount: Integer;
      const OnWriteItem: TJvAppStorageListItemEvent;
      const OnDeleteItems: TJvAppStorageListDeleteEvent = nil;
      const ItemName: string = cItem);

    { Retrieves a list of objects. The list is optionally cleared before before reading starts.
      The ObjectType of the Object is retrieved from the stored "Classname" value.
      The result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadObjectList(const Path: string; List: TList;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer; overload;
    { Retrieves a list of objects. The list is optionally cleared before before reading starts.
      The ObjectType of the Object is defined by the ItemCreator-Event.
      The result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadObjectList(const Path: string; List: TList;
      ItemCreator: TJvAppStorageObjectListItemCreateEvent;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer; overload;
    { Stores a list of objects. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteObjectList(const Path: string; List: TList; const ItemName: string = cItem);

    { Retrieves a list of collection items . The list is optionally cleared before before reading starts.
      The result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadCollection(const Path: string; List: TCollection;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
    { Stores all items of a collection. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteCollection(const Path: string; List: TCollection; const ItemName: string = cItem);


    { Retrieves a string list with addition objects.
      The ObjectType of the Object is retrieved from the stored "Classname" value.
      The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringObjectList(const Path: string; const SL: TStrings;
      const ClearFirst: Boolean = True;
      const ItemName: string = cItem): Integer; overload;
    { Retrieves a string list with addition objects.
      The ObjectType of the Object is defined by the ItemCreator-Event.
      The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringObjectList(const Path: string; const SL: TStrings;
      ItemCreator: TJvAppStorageObjectListItemCreateEvent;
      const ClearFirst: Boolean = True;
      const ItemName: string = cItem): Integer; overload;

    { Stores and also the attached object informations of a string list.
      Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteStringObjectList(const Path: string; const SL: TStrings; const ItemName: string = cItem);
    { Retrieves a string list. The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringList(const Path: string; const SL: TStrings;
      const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
    { Stores a string list. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteStringList(const Path: string; const SL: TStrings; const ItemName: string = cItem);
    { Retrieves an enumeration. If the value is not found, the Default will be returned. }
    procedure ReadEnumeration(const Path: string; TypeInfo: PTypeInfo;
      const Default; out Value);
    { Stores an enumeration }
    procedure WriteEnumeration(const Path: string; TypeInfo: PTypeInfo;
      const Value);
    procedure ReadSet(const Path: string; ATypeInfo: PTypeInfo; const Default; out Value);
    { Stores a set. }
    procedure WriteSet(const Path: string; ATypeInfo: PTypeInfo; const Value);
    { Retrieves the specified Boolean value. If the value is not found, the Default will be
      returned. If the value is not an Boolean (or can't be converted to a Boolean an EConvertError
      exception will be raised. }
    function ReadBoolean(const Path: string; Default: Boolean = True): Boolean;
    { Stores an Boolean value
      The value is stored as string TRUE/FALSE. }
    procedure WriteBoolean(const Path: string; Value: Boolean);
    { Retrieves an Property. If the value is not found, the Property is not changed. }
    procedure ReadProperty(const Path: string; const PersObj: TPersistent;
      const PropName: string; const Recursive, ClearFirst: Boolean);
    { Stores an Property }
    procedure WriteProperty(const Path: string; const PersObj: TPersistent;
      const PropName: string; const Recursive: Boolean);
    { Retrieves a set. If the value is not found, the Default will be returned. }
    { Retrieves a TPersistent-Object with all of its published properties }
    procedure ReadPersistent(const Path: string; const PersObj: TPersistent;
      const Recursive: Boolean = True; const ClearFirst: Boolean = True; const IgnoreProperties: TStrings = nil);
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
      const Options: TJvAppStorageEnumOptions = [aeoValues, aeoReportListAsValue, aeoRecursive]);
    { Enables the Cryption of Property-Values (Only String-Values) }
    procedure EnablePropertyValueCrypt;
    { Disables the Cryption of Property-Values (Only String-Values) }
    procedure DisablePropertyValueCrypt;
    { Returns the current state if Property-Value Cryption is enabled }
    function IsPropertyValueCryptEnabled: Boolean;
    { Root of any values to be read/written. This value is combined with the path given in one of
      the Read*/Write* methods to determine the actual key used. It's always relative to the value
      of Root (which is an absolute path) }
    property Path: string read GetPath write SetPath;
    { Defines if the Storage-Component is readonly or not.
      If Readonly is true all Calls to an Write*-Procedure will be ignored.
      The property is calulated by a combination of setting the
      property ReadOnly and Result of the function GetPhysicalReadOnly }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default false;
  published
    property StorageOptions: TJvCustomAppStorageOptions read FStorageOptions write SetStorageOptions;
    property OnTranslatePropertyName: TJvAppStoragePropTranslateEvent read FOnTranslatePropertyName
      write FOnTranslatePropertyName;
    property OnEncryptPropertyValue: TJvAppStorageCryptEvent read FOnEncryptPropertyValue
      write FOnEncryptPropertyValue;
    property OnDecryptPropertyValue: TJvAppStorageCryptEvent read FOnDecryptPropertyValue
      write FOnDecryptPropertyValue;
  end;

  { Generic store that can only be used to combine various other storages (only storages in the
    SubStorages collection are usable; any references to paths not specified in this collection
    will raise an exception). Can be used for example to provide access to the entire registry
    hive from a single app store component by adding a number of TJvAppRegistryStorage storages,
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
  TJvAppStorage = class(TJvCustomAppStorage)
  protected
    function IsFolderInt(const Path: string; ListIsValue: Boolean = True): Boolean; override;
    function PathExistsInt(const Path: string): Boolean; override;
    function ValueStoredInt(const Path: string): Boolean; override;
    procedure DeleteValueInt(const Path: string); override;
    procedure DeleteSubTreeInt(const Path: string); override;
    function ReadIntegerInt(const Path: string; Default: Integer = 0): Integer; override;
    procedure WriteIntegerInt(const Path: string; Value: Integer); override;
    function ReadFloatInt(const Path: string; Default: Extended = 0): Extended; override;
    procedure WriteFloatInt(const Path: string; Value: Extended); override;
    function ReadStringInt(const Path: string; const Default: string = ''): string; override;
    procedure WriteStringInt(const Path: string; const Value: string); override;
    function ReadBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer): Integer; override;
    procedure WriteBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer); override;
    function ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime; override;
    procedure WriteDateTimeInt(const Path: string; Value: TDateTime); override;
    function ReadBooleanInt(const Path: string; Default: Boolean): Boolean; override;
    procedure WriteBooleanInt(const Path: string; Value: Boolean); override;
    procedure ReadEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Default; out Value); override;
    procedure WriteEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Value); override;
    procedure ReadSetInt(const Path: string; ATypeInfo: PTypeInfo; const Default; out Value); override;
    procedure WriteSetInt(const Path: string; ATypeInfo: PTypeInfo; const Value); override;
    property ReadOnly;
  published
    property SubStorages;
  end;

  TJvCustomAppStorageOptions = class(TPersistent)
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
    function IsValueListString(const AValue, AList: string): Boolean; virtual;
  public
    constructor Create; virtual;
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

  TJvAppStorageOptions = class(TJvCustomAppStorageOptions)
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

  TJvAppSubStorages = class(TOwnedCollection)
  private
    function GetRootStorage: TJvCustomAppStorage;
    function GetItem(I: Integer): TJvAppSubStorage;
    procedure SetItem(I: Integer; Value: TJvAppSubStorage);
  protected
    { Notify sub storages of a change in the options of the root storage. This allows sub storage
      to be kept in sync with the root storage. }
    procedure RootOptionsChanged;
    { Check if the given root path is unique, optionally ignoring a specific sub storage (eg. when
      modifying the root path of a storage, that storage's RootPath is irrelavant in determining
      if the new name will be unique). }
    function CheckUniqueBase(const APath: string; IgnoreIndex: Integer): Boolean;
    { Retrieves the sub storage for the given root path, optionally ignoring a specific sub storage.
      The specified path is assumed to be at root level (regardless whether the paths starts with
      a backslash (\) or not) and leading and trailing backslashes are removed automatically.
      The last element in the path string is ignored to avoid returning a sub storage for the root
      path itself. To search for a sub store for a root path, simply add '\*' at the end of the
      path. }
    function MatchFor(APath: string; IgnoreIndex: Integer = -1): TJvAppSubStorage;

    property RootStorage: TJvCustomAppStorage read GetRootStorage;
  public
    constructor Create(AOwner: TJvCustomAppStorage);
    procedure Add(RootPath: string; AppStorage: TJvCustomAppStorage);
    procedure Delete(Index: Integer); overload;
    procedure Delete(RootPath: string; const IncludeSubPaths: Boolean = False); overload;
    procedure Delete(AppStorage: TJvCustomAppStorage); overload;

    property Items[I: Integer]: TJvAppSubStorage read GetItem write SetItem; default;
  end;

  TJvAppSubStorage = class(TCollectionItem)
  private
    FRootPath: string;
    FAppStorage: TJvCustomAppStorage;
  protected
    function GetOwnerStore: TJvCustomAppStorage;
    function GetDisplayName: string; override;
    procedure SetRootPath(Value: string);
    procedure SetAppStorage(Value: TJvCustomAppStorage);

    property OwnerStore: TJvCustomAppStorage read GetOwnerStore;
  published
    property RootPath: string read FRootPath write SetRootPath;
    property AppStorage: TJvCustomAppStorage read FAppStorage write SetAppStorage;
  end;

  // Base class for all in memory file storage classes.
  // All descendents implement a file storage, but all changes
  // are left in memory until the Flush method is called.
  // Flush is automatically called by the destructor, but
  // you can override Flush to write the file on a support
  // different from a disk, such as database record.
  // Please note that in the derived class, if you use an object
  // to represent the file in memory, this object MUST be freed
  // AFTER the call to inherited in the destructor of your
  // derived class or Flush would access a deleted object
  TJvCustomAppMemoryFileStorage = class(TJvCustomAppStorage)
  protected
    FFileName: TFileName;
    FLocation: TFileLocation;
    FLoadedFinished: Boolean;
    FOnGetFileName: TJvAppStorageGetFileNameEvent;
    FPhysicalReadOnly: Boolean;

    function GetAsString: string; virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;

    procedure SetFileName(const Value: TFileName);
    procedure SetLocation(const Value: TFileLocation);
    function DefaultExtension: string; virtual;

    function DoGetFileName: TFileName; virtual;
    function GetFullFileName: TFileName;

    property AsString: string read GetAsString write SetAsString;
    property FileName: TFileName read FFileName write SetFileName;
    property Location: TFileLocation read FLocation write SetLocation default flExeFile;

    property OnGetFileName: TJvAppStorageGetFileNameEvent
      read FOnGetFileName write FOnGetFileName;
      // OnGetFileName triggered on Location = flCustom

    function GetPhysicalReadOnly : Boolean; override;

    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Reload; override;

    property FullFileName: TFileName read GetFullFileName;
  published
    property ReadOnly;
  end;

// (marcelb) moved back; the constants are useful to the outside world after a call to GetStoredValues
// (rom) give it better names and delete these comments :-)
const
  aptFolder = 1;
  aptValue  = 2;



implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING} 
  JclFileUtils, JclStrings, JclSysInfo, JclRTTI, JclMime,
  JvQPropertyStore, JvQConsts, JvQResources;

const
  // (rom) this name is shared in several units and should be made global
  cCount = 'Count';
  cObject = 'Object';
  cItemName = 'Itemname';
  cClassName = 'Classname';
  cInvalidIdentifier = ' #!@not known@!# ';
  // (rom) should this be PathDelim + '*' as implemented before i changed it
  // (rom) or \* as comments say?
  cSubStorePath = '\*';



procedure UpdateGlobalPath(GlobalPaths, NewPaths: TStrings);
var
  I: Integer;
  J: Integer;
begin
  for I := 0 to NewPaths.Count - 1 do
  begin
    if StrCharCount(NewPaths[I],'.') = Length(NewPaths[I]) then
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
      while (Index > 0) and (StrLeft(Paths[Index], 1) <> PathDelim) do
        Dec(Index);
      repeat
        StrToStrings(Paths[Index], PathDelim, CurPaths, False);
        UpdateGlobalPath(GlobalPaths, CurPaths);
        Inc(Index);
      until Index > High(Paths);
      Result := StringsToStr(GlobalPaths, PathDelim, False);
      // (hofi) it would be better to trim both ends of path ?!?!?
      // currently only path contains space(s) filtered out
      if Length(Trim(Result)) = 0 then
        Result := '';
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
      Result := Shortint(Value);
    otUByte:
      Result := Byte(Value);
    otSWord:
      Result := Smallint(Value);
    otUWord:
      Result := Word(Value);
    otSLong, otULong:
      Result := Longint(Value);
    else
      Result := -1;
  end;
end;

//=== { TJvCustomAppStorageOptions } =========================================

constructor TJvCustomAppStorageOptions.Create;
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

function TJvCustomAppStorageOptions.IsValueListString(const AValue, AList: string): Boolean;
begin
  with TStringList.Create do
    try
      CommaText := UpperCase(AList);
      Result := IndexOf(UpperCase(AValue)) >= 0;
    finally
      Free;
    end;
end;

function TJvCustomAppStorageOptions.DefaultTrueString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringTrueValues);
  if I = 0 then
    I := Length(FBooleanStringTrueValues) + 1;
  Result := Trim(Copy(FBooleanStringTrueValues, 1, I - 1));
end;

function TJvCustomAppStorageOptions.DefaultFalseString: string;
var
  I: Integer;
begin
  I := Pos(',', FBooleanStringFalseValues);
  if I = 0 then
    I := Length(FBooleanStringFalseValues) + 1;
  Result := Trim(Copy(FBooleanStringFalseValues, 1, I - 1));
end;

function TJvCustomAppStorageOptions.IsValueTrueString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringTrueValues);
end;

function TJvCustomAppStorageOptions.IsValueFalseString(Value: string): Boolean;
begin
  Result := IsValueListString(Value, FBooleanStringFalseValues);
end;

procedure TJvCustomAppStorageOptions.SetBooleanAsString(Value: Boolean);
begin
  FBooleanAsString := Value and (DefaultTrueString <> '') and (DefaultFalseString <> '');
end;

procedure TJvCustomAppStorageOptions.SetBooleanStringTrueValues(Value: string);
begin
  FBooleanStringTrueValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultTrueString <> '')
end;

procedure TJvCustomAppStorageOptions.SetBooleanStringFalseValues(Value: string);
begin
  FBooleanStringFalseValues := Value;
  FBooleanAsString := FBooleanAsString and (DefaultFalseString <> '')
end;

procedure TJvCustomAppStorageOptions.SetEnumAsStr(Value: Boolean);
begin
  FEnumAsStr := Value;
end;

procedure TJvCustomAppStorageOptions.SetIntAsStr(Value: Boolean);
begin
  FIntAsStr := Value;
end;

procedure TJvCustomAppStorageOptions.SetSetAsStr(Value: Boolean);
begin
  FSetAsStr := Value;
end;

procedure TJvCustomAppStorageOptions.SetDateTimeAsStr(Value: Boolean);
begin
  FDateTimeAsString := Value;
end;

procedure TJvCustomAppStorageOptions.SetFloatAsStr(Value: Boolean);
begin
  FFloatAsString := Value;
end;

procedure TJvCustomAppStorageOptions.SetDefaultIfReadConvertError(Value: Boolean);
begin
  FDefaultIfReadConvertError := Value;
end;

procedure TJvCustomAppStorageOptions.SetDefaultIfValueNotExists(Value: Boolean);
begin
  FDefaultIfValueNotExists := Value;
end;

//=== { TJvCustomAppStorage } ================================================

constructor TJvCustomAppStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoFlush := False;
  FAutoReload := False;
  FStorageOptions := GetStorageOptionsClass.Create;
  FSubStorages := TJvAppSubStorages.Create(Self);
  FCryptEnabledStatus := 0;
  FReadOnly := False;
end;

destructor TJvCustomAppStorage.Destroy;
begin
  Flush;
  FreeAndNil(FSubStorages);
  FreeAndNil(FStorageOptions);
  inherited Destroy;
end;

procedure TJvCustomAppStorage.Flush;
begin
  // do nothing
end;

procedure TJvCustomAppStorage.Reload;
begin
  // do nothing
end;

procedure TJvCustomAppStorage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent is TJvCustomAppStorage) and (Operation = opRemove) and
    Assigned(SubStorages) then
    SubStorages.Delete(AComponent as TJvCustomAppStorage);
end;

function TJvCustomAppStorage.GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data   := GetTypeData(Instance.ClassInfo);
  Result := Data^.PropCount;
end;

function TJvCustomAppStorage.GetPropName(Instance: TPersistent; Index: Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  GetMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  end;
end;

class function TJvCustomAppStorage.GetStorageOptionsClass: TJvAppStorageOptionsClass;
begin
  Result := TJvAppStorageOptions;
end;

procedure TJvCustomAppStorage.SplitKeyPath(const Path: string; out Key, ValueName: string);
var
  AbsPath: string;
  ValueNamePos: Integer;
begin
  AbsPath := GetAbsPath(Path);
  ValueNamePos := LastDelimiter(PathDelim, AbsPath);
  Key := StrLeft(AbsPath, ValueNamePos - 1);
  ValueName := StrRestOf(AbsPath, ValueNamePos + 1);
end;

procedure TJvCustomAppStorage.SetSubStorages(Value: TJvAppSubStorages);
begin
end;

function TJvCustomAppStorage.GetRoot: string;
begin
  Result := FRoot;
end;

procedure TJvCustomAppStorage.SetRoot(const Value: string);
begin
  FRoot := OptimizePaths([Value]);
end;

function TJvCustomAppStorage.GetCurrentPath: string;
begin
  Result := GetAbsPath('');
end;

function TJvCustomAppStorage.GetAbsPath(const Path: string): string;
begin
  Result := GetRoot + PathDelim + OptimizePaths([GetPath, Path]);
  while (Result <> '') and (Result[1] = PathDelim) do
    Delete(Result, 1, 1);
end;

procedure TJvCustomAppStorage.ReadStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TStrings then
    TStrings(List).Add(Sender.ReadString(ConcatPaths([Path, ItemName + IntToStr(Index)])));
end;

procedure TJvCustomAppStorage.WriteStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TStrings then
    Sender.WriteString(ConcatPaths([Path, ItemName + IntToStr(Index)]), TStrings(List)[Index]);
end;

procedure TJvCustomAppStorage.DeleteStringListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TStrings then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

function TJvCustomAppStorage.DefaultObjectListItemCreateEvent(Sender: TJvCustomAppStorage; const Path: string; Index: Integer): TPersistent;
var
  NewClassName: string;
begin
  NewClassName := Sender.ReadString(ConcatPaths([Path, cClassName]));
  Result := TPersistent(GetClass(NewClassName).Create);
end;

procedure TJvCustomAppStorage.ReadObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
begin
  if List is TList then
  begin
    try
      NewPath := ConcatPaths([Path, ItemName + IntToStr(Index)]);
      NewItem := FCurrentInstanceCreateEvent(Sender, NewPath, Index);
      TList(List).Add(NewItem);
      Sender.ReadPersistent(NewPath, NewItem);
    except
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TList then
    if Assigned(TList(List)[Index]) then
      Sender.WritePersistent(ConcatPaths([Path, ItemName + IntToStr(Index)]), TPersistent(TList(List)[Index]));
end;

procedure TJvCustomAppStorage.DeleteObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TList then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

procedure TJvCustomAppStorage.ReadStringObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
  NewName: string;
begin
  if List is TStrings then
  begin
    try
      NewPath := ConcatPaths([Path, ItemName + IntToStr(Index)]);
      NewItem := FCurrentInstanceCreateEvent(Sender, ConcatPaths([NewPath, cObject]), Index);
      NewName := Sender.ReadString(ConcatPaths([NewPath, cItemName]));
      TStrings(List).AddObject(NewName, NewItem);
      if NewItem is TJvCustomPropertyStore then
        Sender.ReadPersistent(ConcatPaths ([NewPath, cObject]), NewItem,
          True, True, TJvCustomPropertyStore(NewItem).CombinedIgnoreProperties)
      else
        Sender.ReadPersistent(ConcatPaths([NewPath, cObject]), NewItem);
    except
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteStringObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  Obj: TObject;
begin
  if List is TStrings then
  begin
    Sender.WriteString(ConcatPaths([Path, ItemName + IntToStr(Index), cItemName]), TStrings(List)[Index]);
    Obj := TStrings(List).Objects[Index];
    if Assigned(Obj) then
      if Obj is TJvCustomPropertyStore then
        Sender.WritePersistent(ConcatPaths([Path, ItemName + IntToStr(Index), cObject]), TPersistent(Obj),
          True, TJvCustomPropertyStore(Obj).CombinedIgnoreProperties)
      else
        Sender.WritePersistent(ConcatPaths([Path, ItemName + IntToStr(Index), cObject]), TPersistent(Obj));
  end;
end;

procedure TJvCustomAppStorage.DeleteStringObjectListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TStrings then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

procedure TJvCustomAppStorage.ReadCollectionItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
begin
  if List is TCollection then
  begin
    try
      NewPath := ConcatPaths([Path, ItemName + IntToStr(Index)]);
      NewItem := TCollection(List).Add;
      if NewItem is TJvCustomPropertyStore then
        Sender.ReadPersistent(NewPath, NewItem, True, True,
          TJvCustomPropertyStore(NewItem).CombinedIgnoreProperties)
      else
        Sender.ReadPersistent(NewPath, NewItem);
    except
    end;
  end;
end;

procedure TJvCustomAppStorage.WriteCollectionItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  Item: TObject;
begin
  if List is TCollection then
  begin
    Item := TCollection(List).Items[Index];
    if Assigned(Item) then
      if Item is TJvCustomPropertyStore then
        Sender.WritePersistent(ConcatPaths([Path, ItemName + IntToStr(Index)]), TPersistent(Item),
          True, TJvCustomPropertyStore(Item).CombinedIgnoreProperties)
      else
        Sender.WritePersistent(ConcatPaths([Path, ItemName + IntToStr(Index)]), TPersistent(Item));
  end;
end;

procedure TJvCustomAppStorage.DeleteCollectionItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TCollection then
    for I := First to Last do
      Sender.DeleteValue(ConcatPaths([Path, ItemName + IntToStr(I)]));
end;


procedure TJvCustomAppStorage.InternalGetStoredValues(const PrefixPath, SearchPath: string;
  const Strings: TStrings; const Options: TJvAppStorageEnumOptions);
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
        if (aeoFolders in Options) and IsFolder(SearchPath + PathDelim +
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
          InternalGetStoredValues(PrefixPath + TempList[I] + PathDelim,
            SearchPath + PathDelim + TempList[I],
            Strings, Options);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

function TJvCustomAppStorage.GetPath: string;
begin
  Result := FCurPath;
end;

procedure TJvCustomAppStorage.SetPath(const Path: string);
begin
  FCurPath := OptimizePaths([Path]);
end;

procedure TJvCustomAppStorage.SetStorageOptions(Value: TJvCustomAppStorageOptions);
begin
  if (Value <> nil) and (Value <> FStorageOptions) then
    FStorageOptions.Assign(Value);
end;

procedure TJvCustomAppStorage.DoTranslatePropertyName(Instance: TPersistent; var Name: string;
  const Reading: Boolean);
begin
  if Assigned(FOnTranslatePropertyName) then
    FOnTranslatePropertyName(Self, Instance, Name, Reading);
end;

function TJvCustomAppStorage.HasSubStorage(AStore: TJvCustomAppStorage): Boolean;
var
  I: Integer;
begin
  I := SubStorages.Count - 1;
  Result := False;
  while not Result and (I >= 0) do
  begin
    Result := (SubStorages[I].AppStorage = AStore) or
      ((SubStorages[I].AppStorage <> nil) and SubStorages[I].AppStorage.HasSubStorage(AStore));
    Dec(I);
  end;
end;

function TJvCustomAppStorage.ListStoredInt(const Path: string): Boolean;
begin
  Result := ValueStoredInt(StrEnsureSuffix(PathDelim, Path) + cCount);
end;

function TJvCustomAppStorage.DoReadDateTime(const Path: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadFloat(Path, Default);
end;

procedure TJvCustomAppStorage.DoWriteDateTime(const Path: string; Value: TDateTime);
begin
  DoWriteFloat(Path, Value);
end;

function TJvCustomAppStorage.DoReadBoolean(const Path: string; Default: Boolean): Boolean;
begin
  Result := DoReadInteger(Path, Ord(Default)) <> Ord(False);
end;

procedure TJvCustomAppStorage.DoWriteBoolean(const Path: string; Value: Boolean);
begin
  DoWriteInteger(Path, Ord(Value));
end;

function TJvCustomAppStorage.ReadIntegerInt(const Path: string; Default: Integer): Integer;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      Result := DoReadInteger(Path, Default);
    except
      on E: EConvertError do
        if StorageOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStorage.WriteIntegerInt(const Path: string; Value: Integer);
begin
  DoWriteInteger(Path, Value);
end;

function TJvCustomAppStorage.ReadFloatInt(const Path: string; Default: Extended): Extended;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      if StorageOptions.FloatAsString then
        try
          Result := StrToFloat(DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(FloatToStr(Default)))));
        except
          on E: EConvertError do
            Result := DoReadFloat(Path, Default);
        end
      else
        try
          Result := DoReadFloat(Path, Default);
        except
          on E: EConvertError do
            Result := StrToFloat(DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(FloatToStr(Default)))));
        end
    except
      on E: EConvertError do
        if StorageOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStorage.WriteFloatInt(const Path: string; Value: Extended);
begin
  if StorageOptions.FloatAsString then
    DoWriteString(Path, EncryptPropertyValue(FloatToStr(Value)))
  else
    DoWriteFloat(Path, Value);
end;

function TJvCustomAppStorage.ReadStringInt(const Path: string; const Default: string): string;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      Result := DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(Default)));
    except
      on E: EConvertError do
        if StorageOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStorage.WriteStringInt(const Path: string; const Value: string);
begin
  DoWriteString(Path, EncryptPropertyValue(Value));
end;

function TJvCustomAppStorage.ReadBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer): Integer;
begin
  Result := DoReadBinary(Path, Buf, BufSize);
end;

procedure TJvCustomAppStorage.WriteBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer);
begin
  DoWriteBinary(Path, Buf, BufSize);
end;

function TJvCustomAppStorage.ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      if StorageOptions.DateTimeAsString then
        try
          Result := StrToDateTime(DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(DateTimeToStr(Default)))));
        except
          on E: EConvertError do
            Result := DoReadDateTime(Path, Default);
        end
      else
        try
          Result := DoReadDateTime(Path, Default);
        except
          on E: EConvertError do
            Result := StrToDateTime(DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(DateTimeToStr(Default)))));
        end
    except
      on E: EConvertError do
        if StorageOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStorage.WriteDateTimeInt(const Path: string; Value: TDateTime);
begin
  if StorageOptions.DateTimeAsString then
    DoWriteString(Path, EncryptPropertyValue(DateTimeToStr(Value)))
  else
    DoWriteFloat(Path, Value);
end;

function TJvCustomAppStorage.ReadBooleanInt(const Path: string; Default: Boolean): Boolean;
var
  Value: string;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
    Result := Default
  else
    try
      if StorageOptions.BooleanAsString then
        try
          if Default then
            Value := DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(StorageOptions.DefaultTrueString)))
          else
            Value := DecryptPropertyValue(DoReadString(Path, EncryptPropertyValue(StorageOptions.DefaultFalseString)));
          if StorageOptions.IsValueTrueString(Value) then
            Result := True
          else
            if StorageOptions.IsValueFalseString(Value) then
              Result := False
            else
              Result := DoReadBoolean(Path, Default);
        except
          on E: EConvertError do
            Result := DoReadBoolean(Path, Default);
        end
      else
        Result := DoReadBoolean(Path, Default);
    except
      on E: EConvertError do
        if StorageOptions.DefaultIfReadConvertError then
          Result := Default
        else
          raise;
    end;
end;

procedure TJvCustomAppStorage.WriteBooleanInt(const Path: string; Value: Boolean);
begin
  if StorageOptions.BooleanAsString then
    if Value then
      DoWriteString(Path, EncryptPropertyValue(StorageOptions.DefaultTrueString))
    else
      DoWriteString(Path, EncryptPropertyValue(StorageOptions.DefaultFalseString))
  else
    DoWriteBoolean(Path, Value);
end;

class function TJvCustomAppStorage.NameIsListItem(const Name: string): Boolean;
var
  NameStart: PChar;
begin
  NameStart := AnsiStrRScan(PChar(Name), PathDelim);
  if NameStart = nil then
    NameStart := PChar(Name);
  Result := (AnsiStrLIComp(NameStart, cItem, 4) = 0) and (NameStart[4] in DigitSymbols);
end;

class function TJvCustomAppStorage.ConcatPaths(const Paths: array of string): string;
begin
  Result := OptimizePaths(Paths);
end;

procedure TJvCustomAppStorage.ResolvePath(const InPath: string; out TargetStore: TJvCustomAppStorage;
  out TargetPath: string);
var
  SubStorageItem: TJvAppSubStorage;
begin
  TargetPath := PathDelim + ConcatPaths([Path, InPath]);
  TargetStore := Self;
  SubStorageItem := SubStorages.MatchFor(TargetPath);
  if (SubStorageItem <> nil) and (SubStorageItem.AppStorage <> nil) then
  begin
    TargetStore := SubStorageItem.AppStorage;
    Delete(TargetPath, 1, Length(SubStorageItem.RootPath) + 1);
    TargetPath := PathDelim + OptimizePaths([TargetPath]);
    if TargetPath = PathDelim then
      raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
  end;
end;

function TJvCustomAppStorage.IsFolder(const Path: string; ListIsValue: Boolean): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.IsFolderInt(TargetPath, ListIsValue);
end;

function TJvCustomAppStorage.PathExists(const Path: string): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.PathExistsInt(TargetPath);
end;

function TJvCustomAppStorage.ValueStored(const Path: string): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ValueStoredInt(TargetPath);
end;

function TJvCustomAppStorage.ListStored(const Path: string): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ListStoredInt(TargetPath);
end;

procedure TJvCustomAppStorage.DeleteValue(const Path: string);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.DeleteValueInt(TargetPath);
end;

procedure TJvCustomAppStorage.DeleteSubTree(const Path: string);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.DeleteSubTreeInt(Path);
end;

function TJvCustomAppStorage.ReadInteger(const Path: string; Default: Integer): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadIntegerInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteInteger(const Path: string; Value: Integer);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteIntegerInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadFloat(const Path: string; Default: Extended): Extended;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadFloatInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteFloat(const Path: string; Value: Extended);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteFloatInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadString(const Path: string; const Default: string): string;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadStringInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteString(const Path: string; const Value: string);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteStringInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadBinary(const Path: string; Buf: Pointer; BufSize: Integer): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadBinaryInt(TargetPath, Buf, BufSize);
end;

procedure TJvCustomAppStorage.WriteBinary(const Path: string; Buf: Pointer; BufSize: Integer);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteBinaryInt(TargetPath, Buf, BufSize);
end;

function TJvCustomAppStorage.ReadDateTime(const Path: string; Default: TDateTime): TDateTime;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadDateTimeInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteDateTime(const Path: string; Value: TDateTime);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteDateTimeInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadBoolean(const Path: string; Default: Boolean): Boolean;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  Result := TargetStore.ReadBooleanInt(TargetPath, Default);
end;

procedure TJvCustomAppStorage.WriteBoolean(const Path: string; Value: Boolean);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteBooleanInt(TargetPath, Value);
end;

function TJvCustomAppStorage.ReadList(const Path: string; const List: TObject;
  const OnReadItem: TJvAppStorageListItemEvent;
  const ItemName: string = cItem): Integer;
var
  I: Integer;
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
  ItemCount: Integer;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  ItemCount := TargetStore.ReadIntegerInt(ConcatPaths([TargetPath, cCount]), 0);
  for I := 0 to ItemCount - 1 do
    OnReadItem(TargetStore, TargetPath, List, I, ItemName);
  Result := ItemCount;
end;

procedure TJvCustomAppStorage.WriteList(const Path: string; const List: TObject;
  const ItemCount: Integer;
  const OnWriteItem: TJvAppStorageListItemEvent;
  const OnDeleteItems: TJvAppStorageListDeleteEvent = nil;
  const ItemName: string = cItem);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
  PrevListCount: Integer;
  I: Integer;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
  begin
    Delete(TargetPath, Length(TargetPath) - 1, 2);
    PrevListCount := TargetStore.ReadIntegerInt(ConcatPaths([TargetPath, cCount]), 0);
    TargetStore.WriteIntegerInt(ConcatPaths([TargetPath, cCount]), ItemCount);
    for I := 0 to ItemCount - 1 do
      OnWriteItem(TargetStore, TargetPath, List, I, ItemName);
    if (PrevListCount > ItemCount) and Assigned(OnDeleteItems) then
      OnDeleteItems(TargetStore, TargetPath, List, ItemCount, PrevListCount - 1, ItemName);
  end;
end;

function TJvCustomAppStorage.ReadObjectList(const Path: string; List: TList;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
begin
  Result := ReadObjectList(Path, List, DefaultObjectListItemCreateEvent, ClearFirst, ItemName);
end;

function TJvCustomAppStorage.ReadObjectList(const Path: string; List: TList;
  ItemCreator: TJvAppStorageObjectListItemCreateEvent;
  const ClearFirst: Boolean = True;
  const ItemName: string = cItem): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  if ClearFirst then
    List.Clear;
  TargetStore.FCurrentInstanceCreateEvent := ItemCreator;
  Result := TargetStore.ReadList(TargetPath, List, TargetStore.ReadObjectListItem, ItemName);
  TargetStore.FCurrentInstanceCreateEvent := nil;
end;

procedure TJvCustomAppStorage.WriteObjectList(const Path: string; List: TList;
  const ItemName: string = cItem);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  TargetStore.WriteList(TargetPath, List, List.Count,
    TargetStore.WriteObjectListItem, TargetStore.DeleteObjectListItem, ItemName);
end;

function TJvCustomAppStorage.ReadCollection(const Path: string; List: TCollection;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  if ClearFirst then
    List.Clear;
  Result := TargetStore.ReadList(TargetPath, List, TargetStore.ReadCollectionItem, ItemName);
end;

procedure TJvCustomAppStorage.WriteCollection(const Path: string;
  List: TCollection; const ItemName: string = cItem);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  TargetStore.WriteList(TargetPath, List, List.Count,
    TargetStore.WriteCollectionItem, TargetStore.DeleteCollectionItem, ItemName);
end;

function TJvCustomAppStorage.ReadStringList(const Path: string; const SL: TStrings;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  SL.BeginUpdate;
  try
    ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
    Delete(TargetPath, Length(TargetPath) - 1, 2);
    if ClearFirst then
      SL.Clear;
    Result := TargetStore.ReadList(TargetPath, SL, TargetStore.ReadStringListItem, ItemName);
  finally
    SL.EndUpdate;
  end;
end;

procedure TJvCustomAppStorage.WriteStringList(const Path: string;
  const SL: TStrings; const ItemName: string = cItem);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  TargetStore.WriteList(TargetPath, SL, SL.Count,
    TargetStore.WriteStringListItem, TargetStore.DeleteStringListItem, ItemName);
end;

function TJvCustomAppStorage.ReadStringObjectList(const Path: string; const SL: TStrings;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
begin
  Result := ReadStringObjectList(Path, SL, DefaultObjectListItemCreateEvent, ClearFirst, ItemName);
end;

function TJvCustomAppStorage.ReadStringObjectList(const Path: string;
  const SL: TStrings; ItemCreator: TJvAppStorageObjectListItemCreateEvent;
  const ClearFirst: Boolean = True; const ItemName: string = cItem): Integer;
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  SL.BeginUpdate;
  try
    ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
    Delete(TargetPath, Length(TargetPath) - 1, 2);
    if ClearFirst then
      SL.Clear;
    Result := TargetStore.ReadList(TargetPath, SL, TargetStore.ReadStringObjectListItem, ItemName);
  finally
    SL.EndUpdate;
  end;
end;

procedure TJvCustomAppStorage.WriteStringObjectList(const Path: string; const SL: TStrings; const ItemName: string = cItem);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path + cSubStorePath, TargetStore, TargetPath);
  Delete(TargetPath, Length(TargetPath) - 1, 2);
  TargetStore.WriteList(TargetPath, SL, SL.Count, TargetStore.WriteStringObjectListItem, TargetStore.DeleteStringObjectListItem, ItemName);
end;

procedure TJvCustomAppStorage.ReadEnumerationInt(const Path: string;
  TypeInfo: PTypeInfo; const Default; out Value);
var
  OrdValue: Integer;
  Conv: TIdentToInt;
  S: string;
  TmpDefReadError: Boolean;
begin
  if not ValueStoredInt(Path) and StorageOptions.DefaultIfValueNotExists then
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
          { Could be stored as a normal int or as an identifier.
            Try identifier first as that will not raise an exception }
          Conv := FindIdentToInt(TypeInfo);
          if Assigned(Conv) then
          begin
            TmpDefReadError := StorageOptions.DefaultIfReadConvertError;
            StorageOptions.DefaultIfReadConvertError := True;
            try
              S := ReadStringInt(Path, '');
            finally
              StorageOptions.DefaultIfReadConvertError := TmpDefReadError;
            end;
            if (S = '') or not (Conv(S, OrdValue)) then
              OrdValue := ReadIntegerInt(Path, OrdValue);
          end
          else
            OrdValue := ReadIntegerInt(Path, OrdValue);
        end
        else
        if TypeInfo.Kind = tkEnumeration then
        begin
          // Usage of an invalid identifier to signal the value does not exist
          OrdValue := GetEnumValue(TypeInfo, ReadStringInt(Path, cInvalidIdentifier));
          if OrdValue = -1 then
            OrdValue := ReadIntegerInt(Path, OrdValue);
        end
        else
          raise EJVCLAppStorageError.CreateRes(@RsEInvalidType);
      except
        on E: EConvertError do
          if StorageOptions.DefaultIfReadConvertError then
            CopyEnumValue(Default, OrdValue, GetTypeData(TypeInfo).OrdType)
          else
            raise;
      end;
    end;
    CopyEnumValue(OrdValue, Value, GetTypeData(TypeInfo).OrdType);
  end;
end;

procedure TJvCustomAppStorage.WriteEnumerationInt(const Path: string;
  TypeInfo: PTypeInfo; const Value);
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
    if StorageOptions.TypedIntegerAsString then
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
    if StorageOptions.EnumerationAsString then
      WriteStringInt(Path, GetEnumName(TypeInfo, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType)))
    else
      WriteIntegerInt(Path, OrdOfEnum(Value, GetTypeData(TypeInfo).OrdType));
  end
  else
    raise EJVCLAppStorageError.CreateRes(@RsEInvalidType);
end;

procedure TJvCustomAppStorage.ReadEnumeration(const Path: string;
  TypeInfo: PTypeInfo; const Default; out Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  TargetStore.ReadEnumerationInt(TargetPath, TypeInfo, Default, Value);
end;

procedure TJvCustomAppStorage.WriteEnumeration(const Path: string;
  TypeInfo: PTypeInfo; const Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteEnumerationInt(TargetPath, TypeInfo, Value);
end;

procedure TJvCustomAppStorage.ReadSetInt(const Path: string;
  ATypeInfo: PTypeInfo; const Default; out Value);
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
              with ((JclTypeInfo(ATypeInfo) as IJclSetTypeInfo).BaseType as IJclEnumerationTypeInfo) do
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
            raise EJVCLAppStorageError.CreateRes(@RsEUnknownBaseType);
        end;
      end;
    finally
      FreeAndNil(Lst);
    end;
  end
  else
    // It's stored as a string value or not stored at all
    JclStrToSet(ATypeInfo, Value, ReadStringInt(Path, JclSetToStr(ATypeInfo, Default, True)));
end;

procedure TJvCustomAppStorage.WriteSetInt(const Path: string;
  ATypeInfo: PTypeInfo; const Value);
var
  Lst: TStrings;
  I: Integer;
begin
  if StorageOptions.SetAsString then
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
            raise EJVCLAppStorageError.CreateRes(@RsEUnknownBaseType);
        end;
      end;
    finally
      FreeAndNil(Lst);
    end;
  end;
end;

procedure TJvCustomAppStorage.ReadSet(const Path: string;
  ATypeInfo: PTypeInfo; const Default; out Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  TargetStore.ReadSetInt(TargetPath, ATypeInfo, Default, Value);
end;

procedure TJvCustomAppStorage.WriteSet(const Path: string;
  ATypeInfo: PTypeInfo; const Value);
var
  TargetStore: TJvCustomAppStorage;
  TargetPath: string;
begin
  ResolvePath(Path, TargetStore, TargetPath);
  if not TargetStore.ReadOnly then
    TargetStore.WriteSetInt(TargetPath, ATypeInfo, Value);
end;


procedure TJvCustomAppStorage.ReadProperty(const Path: string;
 const PersObj: TPersistent; const PropName: string; const Recursive, ClearFirst: Boolean);
var
  //Index: Integer;
  TmpValue: Integer;
  SubObj: TObject;
begin
  if not Assigned(PersObj) then
    Exit;
  case PropType(PersObj, PropName) of
    tkLString, tkWString, tkString:
      SetStrProp(PersObj, PropName, ReadString(Path, GetStrProp(PersObj, PropName)));
    tkEnumeration:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        ReadEnumeration(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
        SetOrdProp(PersObj, PropName, TmpValue);
      end;
    tkSet:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        ReadSet(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
        SetOrdProp(PersObj, PropName, TmpValue);
      end;
    tkChar, tkInteger:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        ReadEnumeration(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue, TmpValue);
        SetOrdProp(PersObj, PropName, TmpValue);
      end;
    tkInt64:
      SetInt64Prop(PersObj, PropName, StrToInt64(ReadString(Path,
        IntToStr(GetInt64Prop(PersObj, PropName)))));
    tkFloat:
      SetFloatProp(PersObj, PropName, ReadFloat(Path, GetFloatProp(PersObj, PropName)));
    tkClass:
      begin
        SubObj := GetObjectProp(PersObj, PropName);
        if SubObj is TStrings then
          ReadStringList(Path, TStrings(SubObj), ClearFirst)
        else
        if (SubObj is TPersistent) and Recursive then
          if SubObj is TJvCustomPropertyStore then
          begin
            TJvCustomPropertyStore(SubObj).AppStoragePath := Path;
            TJvCustomPropertyStore(SubObj).LoadProperties;
          end
          else if SubObj is TCollection then
            ReadCollection(Path, TCollection(SubObj), ClearFirst)
          else
            ReadPersistent(Path, TPersistent(SubObj), True, ClearFirst);
      end;
  end;
end;

procedure TJvCustomAppStorage.WriteProperty(const Path: string;
  const PersObj: TPersistent; const PropName: string; const Recursive: Boolean);
var
  TmpValue: Integer;
  SubObj: TObject;
begin
  if not Assigned(PersObj) then
    Exit;
  case PropType(PersObj, PropName) of
    tkLString, tkWString, tkString:
      WriteString(Path, GetStrProp(PersObj, PropName));
    tkEnumeration:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        WriteEnumeration(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue);
      end;
    tkSet:
      begin
        TmpValue := GetOrdProp(PersObj, PropName);
        WriteSet(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue);
      end;
    tkChar, tkInteger:
      begin
        if StorageOptions.TypedIntegerAsString then
        begin
          TmpValue := GetOrdProp(PersObj, PropName);
          WriteEnumeration(Path, GetPropInfo(PersObj, PropName).PropType^, TmpValue);
        end
        else
          WriteInteger(Path, GetOrdProp(PersObj, PropName));
      end;
    tkInt64:
      WriteString(Path, IntToStr(GetInt64Prop(PersObj, PropName)));
    tkFloat:
      WriteFloat(Path, GetFloatProp(PersObj, PropName));
    tkClass:
      begin
        SubObj := GetObjectProp(PersObj, PropName);
        if SubObj is TStrings then
          WriteStringList(Path, TStrings(SubObj))
        else
        if (SubObj is TPersistent) and Recursive then
          if SubObj is TJvCustomPropertyStore then
          begin
            TJvCustomPropertyStore(SubObj).AppStoragePath := Path;
            TJvCustomPropertyStore(SubObj).AppStorage := Self;
            TJvCustomPropertyStore(SubObj).StoreProperties;
          end
          else if SubObj is TCollection then
            WriteCollection(Path, TCollection(SubObj))
          else
            WritePersistent(Path, TPersistent(SubObj), Recursive, nil);
      end;
  end;
end;

procedure TJvCustomAppStorage.ReadPersistent(const Path: string; const PersObj: TPersistent;
  const Recursive, ClearFirst: Boolean; const IgnoreProperties: TStrings);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  JvAppStorageHandler: IJvAppStorageHandler;
begin
  if not Assigned(PersObj) then
    Exit;
  if Supports(PersObj, IJvAppStorageHandler, JvAppStorageHandler)then
    JvAppStorageHandler.ReadFromAppStorage(Self, Path);
  if not Supports(PersObj, IJvAppStorageHandler) or
    Supports(PersObj, IJvAppStoragePublishedProps) then
    for Index := 0 to GetPropCount(PersObj) - 1 do
    begin
      PropName := GetPropName(PersObj, Index);
      KeyName := TranslatePropertyName(PersObj, PropName, False);
      PropPath := ConcatPaths([Path, KeyName]);
      if (IgnoreProperties = nil) or (IgnoreProperties.IndexOf(PropName) = -1) then
        ReadProperty(PropPath, PersObj, PropName, Recursive, ClearFirst);
    end;
end;

procedure TJvCustomAppStorage.WritePersistent(const Path: string;
  const PersObj: TPersistent; const Recursive: Boolean; const IgnoreProperties: TStrings);
var
  Index: Integer;
  PropName: string;
  KeyName: string;
  PropPath: string;
  JvAppStorageHandler: IJvAppStorageHandler;
begin
  if not Assigned(PersObj) then
    Exit;
  if Supports(PersObj, IJvAppStorageHandler, JvAppStorageHandler)then
    JvAppStorageHandler.WriteToAppStorage(Self, Path);
  if not Supports(PersObj, IJvAppStorageHandler) or
    Supports(PersObj, IJvAppStoragePublishedProps) then
    for Index := 0 to GetPropCount(PersObj) - 1 do
    begin
      PropName := GetPropName(PersObj, Index);
      KeyName := TranslatePropertyName(PersObj, PropName, False);
      PropPath := ConcatPaths([Path, KeyName]);
      if (IgnoreProperties = nil) or (IgnoreProperties.IndexOf(PropName) = -1) then
        WriteProperty(PropPath, PersObj, PropName, Recursive);
    end;
end;

function TJvCustomAppStorage.GetCharName(Ch: Char): string;
begin
  if Ch in ['!' .. 'z'] then
    Result := 'Char_' + Ch
  else
    Result := 'Char#' + IntToStr(Ord(Ch));
end;

function TJvCustomAppStorage.GetIntName(Value: Integer): string;
begin
  Result := 'Int_' + IntToStr(Value);
end;

function TJvCustomAppStorage.EncryptPropertyValue(Value: string): string;
begin
  if Assigned(FOnEncryptPropertyValue) and IsPropertyValueCryptEnabled then
  begin
    FOnEncryptPropertyValue(Value);
    Value := MimeEncodeString(Value);
  end;
  Result := Value;
end;

function TJvCustomAppStorage.DecryptPropertyValue(Value: string): string;
begin
  if Assigned(FOnDecryptPropertyValue) and IsPropertyValueCryptEnabled then
  begin
    Value := MimeDecodeString(Value);
    FOnDecryptPropertyValue(Value);
  end;
  Result := Value;
end;

function TJvCustomAppStorage.TranslatePropertyName(Instance: TPersistent;
  const AName: string; const Reading: Boolean): string;
begin
  Result := AName;
  if Instance is TJvCustomPropertyStore then
    Result := TJvCustomPropertyStore(Instance).TranslatePropertyName(Result)
  else
    DoTranslatePropertyName(Instance, Result, Reading);
end;

procedure TJvCustomAppStorage.SetReadOnly (Value: Boolean);
begin
  fReadOnly := Value;
end;

function TJvCustomAppStorage.GetReadOnly: Boolean;
begin
  if csDesigning in ComponentState then
    Result := fReadOnly
  else
    Result := fReadOnly OR GetPhysicalReadOnly;
end;

function TJvCustomAppStorage.GetPhysicalReadOnly : Boolean;
begin
  Result := False;
end;

procedure TJvCustomAppStorage.GetStoredValues(const Path: string;
  const Strings: TStrings; const Options: TJvAppStorageEnumOptions);
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
        PathDelim, SearchPath, Strings, Options);
    I := Strings.IndexOf(OptimizePaths([Self.Path, SearchPath]));
    if I > -1 then
      Strings.Delete(I);
  finally
    Strings.EndUpdate;
  end;
end;

{ Enables the Cryption of Property-Values (Only String-Values) }
procedure TJvCustomAppStorage.EnablePropertyValueCrypt;
begin
  Inc(FCryptEnabledStatus);
end;

{ Disables the Cryption of Property-Values (Only String-Values) }
procedure TJvCustomAppStorage.DisablePropertyValueCrypt;
begin
  Dec(FCryptEnabledStatus);
end;

{ Returns the current state if Property-Value Cryption is enabled }
function TJvCustomAppStorage.IsPropertyValueCryptEnabled: Boolean;
begin
  Result := (FCryptEnabledStatus > 0);
end;

//=== { TJvAppStorage } ======================================================

function TJvAppStorage.IsFolderInt(const Path: string; ListIsValue: Boolean): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.PathExistsInt(const Path: string): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ValueStoredInt(const Path: string): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.DeleteValueInt(const Path: string);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.DeleteSubTreeInt(const Path: string);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadIntegerInt(const Path: string; Default: Integer): Integer;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteIntegerInt(const Path: string; Value: Integer);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadFloatInt(const Path: string; Default: Extended): Extended;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteFloatInt(const Path: string; Value: Extended);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadStringInt(const Path: string; const Default: string): string;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteStringInt(const Path: string; const Value: string);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer): Integer;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteBinaryInt(const Path: string; Buf: Pointer; BufSize: Integer);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadDateTimeInt(const Path: string; Default: TDateTime): TDateTime;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteDateTimeInt(const Path: string; Value: TDateTime);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

function TJvAppStorage.ReadBooleanInt(const Path: string; Default: Boolean): Boolean;
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteBooleanInt(const Path: string; Value: Boolean);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.ReadEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Default; out Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteEnumerationInt(const Path: string; TypeInfo: PTypeInfo; const Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.ReadSetInt(const Path: string;
  ATypeInfo: PTypeInfo; const Default; out Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

procedure TJvAppStorage.WriteSetInt(const Path: string;
  ATypeInfo: PTypeInfo; const Value);
begin
  raise EJVCLAppStorageError.CreateRes(@RsEInvalidPath);
end;

//=== { TJvAppSubStorages } ==================================================

constructor TJvAppSubStorages.Create(AOwner: TJvCustomAppStorage);
begin
  inherited Create(AOwner, TJvAppSubStorage);
end;

function TJvAppSubStorages.GetRootStorage: TJvCustomAppStorage;
begin
  Result := TJvCustomAppStorage(GetOwner);
end;

function TJvAppSubStorages.GetItem(I: Integer): TJvAppSubStorage;
begin
  Result := TJvAppSubStorage(inherited GetItem(I));
end;

procedure TJvAppSubStorages.SetItem(I: Integer; Value: TJvAppSubStorage);
begin
  inherited SetItem(I, Value);
end;

procedure TJvAppSubStorages.RootOptionsChanged;
begin
end;

function TJvAppSubStorages.CheckUniqueBase(const APath: string; IgnoreIndex: Integer): Boolean;
begin
  Result := MatchFor(OptimizePaths([APath]) + cSubStorePath, IgnoreIndex) = nil;
end;

function TJvAppSubStorages.MatchFor(APath: string; IgnoreIndex: Integer): TJvAppSubStorage;
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
        if APath[Length(Items[I].RootPath) + 1] = PathDelim then
          { Next char in APath is a backslash, so we have a valid match. Check with any previous
            to see if it is better than that one. }
          if (Result = nil) or (Length(Result.RootPath) < Length(Items[I].RootPath)) then
            Result := Items[I]; // no previous match or new match is close to what we searched for
end;

procedure TJvAppSubStorages.Add(RootPath: string; AppStorage: TJvCustomAppStorage);
var
  Tmp: TJvAppSubStorage;
begin
  Tmp := TJvAppSubStorage.Create(Self);
  try
    Tmp.RootPath := RootPath;
    Tmp.AppStorage := AppStorage;
  except
    FreeAndNil(Tmp);
    raise;
  end;
end;

procedure TJvAppSubStorages.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TJvAppSubStorages.Delete(RootPath: string; const IncludeSubPaths: Boolean);
var
  I: Integer;
  SubPath: string;
  CmpLen: Integer;
begin
  RootPath := OptimizePaths([RootPath]);
  if RootPath <> '' then
  begin
    SubPath := RootPath + PathDelim;
    CmpLen := Length(SubPath);
    I := Count - 1;
    while I >= 0 do
    begin
      if AnsiSameText(RootPath, Items[I].RootPath) or
        (IncludeSubPaths and (StrLIComp(PChar(SubPath), PChar(Items[I].RootPath), CmpLen) = 0)) then
        Delete(I);
      Dec(I);
    end;
  end;
end;

procedure TJvAppSubStorages.Delete(AppStorage: TJvCustomAppStorage);
var
  I: Integer;
begin
  I := Count - 1;
  while I >= 0 do
  begin
    if Items[I].AppStorage = AppStorage then
      Delete(I);
    Dec(I);
  end;
end;

//=== { TJvAppSubStorage } ===================================================

function TJvAppSubStorage.GetOwnerStore: TJvCustomAppStorage;
begin
  Result := TJvAppSubStorages(Collection).RootStorage;
end;

function TJvAppSubStorage.GetDisplayName: string;
begin
  if (RootPath <> '') and (AppStorage <> nil) then
    Result := PathDelim + RootPath + '=' + AppStorage.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TJvAppSubStorage.SetRootPath(Value: string);
begin
  Value := OptimizePaths([Value]);
  if Value <> RootPath then
    if TJvAppSubStorages(Collection).CheckUniqueBase(Value, Index) then
      FRootPath := Value
    else
      raise EJVCLAppStorageError.CreateResFmt(@RsENotAUniqueRootPath, [Value]);
end;

procedure TJvAppSubStorage.SetAppStorage(Value: TJvCustomAppStorage);
begin
  if Value <> AppStorage then
  begin
    if (Value <> nil) and (Value.HasSubStorage(OwnerStore) or (Value = OwnerStore)) then
      raise EJVCLAppStorageError.CreateRes(@RsECircularReferenceOfStorages);
    if AppStorage <> nil then
      AppStorage.RemoveFreeNotification(OwnerStore);
    FAppStorage := Value;
    if AppStorage <> nil then
      AppStorage.FreeNotification(OwnerStore);
  end;
end;

//=== { TJvAppStorageFileName } ==============================================

{procedure TJvAppStorageFileName.SetLocation(Value: TFileLocation);
begin
  if Location <> Value then
  begin
    FLocation := Value;
    DoChange;
  end;
end;

procedure TJvAppStorageFileName.SetFileName(Value: TFileName);
begin
  if FileName <> Value then
  begin
    FFileName := Value;
    DoChange;
  end;
end;

procedure TJvAppStorageFileName.DoChange;
begin
  if Assigned(FOnChange) then
    OnChange(Self);
end;

function TJvAppStorageFileName.GetFileName: TFileName;
var
  NameOnly: string;
  RelPathName: string;
begin
  if FileName = '' then
    Result := ''
  else
  begin
    NameOnly := ExtractFileName(FileName);
    if PathIsAbsolute(FileName) then
      RelPathName := NameOnly
    else
      RelPathName := FileName;
    case Location of
      flCustom:
        Result := FileName;
      flTemp:
        Result := PathAddSeparator(GetWindowsTempFolder) + NameOnly;
      flWindows:
        Result := PathAddSeparator(GetWindowsFolder) + NameOnly;
      flExeFile:
        Result := ExtractFilePath(Application.ExeName) + NameOnly;
      flUserFolder:
        Result := PathAddSeparator(GetAppdataFolder) + RelPathName;
    end;
  end;
end;

constructor TJvAppStorageFileName.Create(ADefaultExtension: string);
begin
  inherited Create;
  FLocation := flExeFile;
  FFileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.' + ADefaultExtension);
end;  }

//=== { TJvCustomAppMemoryFileStorage } ======================================

constructor TJvCustomAppMemoryFileStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocation := flExeFile;
  FLoadedFinished := False;
  FPhysicalReadOnly := False;
end;

procedure TJvCustomAppMemoryFileStorage.Loaded;
begin
  inherited Loaded;
  FLoadedFinished := True;
end;

procedure TJvCustomAppMemoryFileStorage.Reload;
begin
  FPhysicalReadOnly := FileExists(FullFileName) and FileIsReadOnly(FullFileName);
  inherited Reload;
end;

function TJvCustomAppMemoryFileStorage.GetPhysicalReadOnly : Boolean;
begin
  Result := FPhysicalReadOnly;
end;

function TJvCustomAppMemoryFileStorage.DoGetFileName: TFileName;
begin
  Result := FileName;
  if Assigned(FOnGetFileName) then
    FOnGetFileName(Self, Result);
end;

function TJvCustomAppMemoryFileStorage.GetFullFileName: TFileName;
var
  NameOnly: string;
  RelPathName: string;
begin
  if FileName = '' then
    Result := ''
  else
  begin
    NameOnly := ExtractFileName(FileName);
    if PathIsAbsolute(FileName) then
      RelPathName := NameOnly
    else
      RelPathName := FileName;
    case Location of
      flCustom:
        Result := DoGetFileName;
      flExeFile:
        Result := PathAddSeparator(ExtractFilePath(ParamStr(0))) + NameOnly;
      {$IFDEF MSWINDOWS}
      flTemp:
        Result := PathAddSeparator(GetWindowsTempFolder) + NameOnly;
      flWindows:
        Result := PathAddSeparator(GetWindowsFolder) + NameOnly;
      flUserFolder:
        Result := PathAddSeparator(GetAppdataFolder) + RelPathName;
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      flTemp:
        Result := PathAddSeparator(GetTempDir) + NameOnly;
      flUserFolder:
        Result := PathAddSeparator(GetEnvironmentVariable('HOME')) + RelPathName;
      {$ENDIF UNIX}
    end;
  end;
end;

procedure TJvCustomAppMemoryFileStorage.SetFileName(const Value: TFileName);
begin
  if FFileName <> PathAddExtension(Value, DefaultExtension) then
  begin
    FFileName := PathAddExtension(Value, DefaultExtension);
    FPhysicalReadOnly := FileExists(FullFileName) and FileIsReadOnly(FullFileName);
    if FLoadedFinished and not IsUpdating then
      Reload;
  end;
end;

procedure TJvCustomAppMemoryFileStorage.SetLocation(const Value: TFileLocation);
begin
  if FLocation <> Value then
  begin
    FLocation := Value;
    FPhysicalReadOnly := FileExists(FullFileName) and FileIsReadOnly(FullFileName);
    if FLoadedFinished and not IsUpdating then
      Reload;
  end;
end;

function TJvCustomAppMemoryFileStorage.DefaultExtension: string;
begin
  Result := '';
end;

procedure TJvCustomAppStorage.Loaded;
begin
  inherited Loaded;
  if not IsUpdating then
    Reload;
end;

procedure TJvCustomAppStorage.BeginUpdate;
begin
  if  not IsUpdating and AutoReload then
    Reload;
  Inc(FUpdateCount);
end;

procedure TJvCustomAppStorage.EndUpdate;
begin
  Dec(FUpdateCount);
  if not IsUpdating and AutoFlush then
    Flush;
  if FUpdateCount < 0 then
    FUpdateCount := 0;
end;

function TJvCustomAppStorage.GetUpdating: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

