{ Insert famous MPL header here... }

unit JvGenStore;

{
  General storage unit - provides with a basic storage component to store application specific
  data. Descendants can provide specific storage to registry, INI-files, DB, XML, etc.
  Should be used to provide a common interface for storing data as is done in some of the JVCL
  components (eg. JvFormPlacement).

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

  This unit is based on the following equation:

    Compilable code + no testing whatsoever = plenty of bugs

  This same equation holds up for the sample implementation for the registry storage in the
  additional unit, but due to the increase in code in that unit, the result will also be higher.
}

interface
                                              
uses
  Classes,
  JvComponent;

type
  TJvCustomAppStore = class;

  TAppStoreListItem = procedure(Sender: TJvCustomAppStore; const Path: string; const Index: Integer) of object;
  TAppStoreListDelete = procedure(Sender: TJvCustomAppStore; const Path: string; const First, Last: Integer) of object;

  TJvCustomAppStore = class(TJvComponent)
  private
    FAppRoot: string;
    FCurRoot: string;
    FStoreSL: TStrings;
  protected
    { Retrieve application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    function GetAppRoot: string; virtual;
    { Set application specific root. Path is prepended to any path specified and serves as an
      absolute root for any storage method. }
    procedure SetAppRoot(Value: string); virtual;
    { Retrieves currently set path (including the AppRoot path). }
    function GetCurrentPath: string;
    { Returns the path as an absolute path (including the AppRoot path). If the given path does not
      start with a backslash (\) the path is appended to the Root path, resolving any references to
      parent folders. }
    function GetAbsPath(Path: string): string;
    { StringList item reader used by ReadStringList in the call to ReadList. }
    procedure ReadSLItem(Sender: TJvCustomAppStore; const Path: string; const Index: Integer);
    { StringList item writer used by WriteStringList in the call to WriteList. }
    procedure WriteSLItem(Sender: TJvCustomAppStore; const Path: string; const Index: Integer);
    { StringList item deleter used by WriteStringList in the call to WriteList. }
    procedure DeleteSLItems(Sender: TJvCustomAppStore; const Path: string; const First, Last: Integer);
    { Current root path for storage. Paths used in other methods are relative to this path. }
    function GetRoot: string; virtual;
    { Specify a new root. Given path is relative to the current path. Se remarks above }
    procedure SetRoot(const Path: string); virtual;

    { Application specific root. Path is prepended to any specified path and serves as an absolute
      root for any reading/writing. Not all implementation will use it. Generally it's used for
      storages not specific to an application (such as the registry). }
    property AppRoot: string read GetAppRoot write SetAppRoot;
    { Root of any values to be read/written. This value is combined with the path given in one of
      the Read*/Write* methods to determine the actual key used. }
    property Root: string read GetRoot write SetRoot;
  public
    { Determines if the specified value is stored }
    function ValueStored(const Path: string): Boolean; virtual; abstract;
    { Deletes the specified value. If the value wasn't stored, nothing will happen. }
    procedure DeleteValue(const Path: string); virtual; abstract;
    { Retrieves the specified Integer value. If the value is not found, the Default will be
      returned. If the value is not an Integer (or can't be converted to an Integer an EConvertError
      exception will be raised. }
    function ReadInteger(const Path: string; Default: Integer = 0): Integer; virtual; abstract;
    { Stores an Integer value. }
    procedure WriteInteger(const Path: string; Value: Integer); virtual; abstract;
    { Retrieves the specified Extended value. If the value is not found, the Default will be
      returned. If the value is not an Extended (or can't be converted to an Extended an
      EConvertError exception will be raised.}
    function ReadFloat(const Path: string; Default: Extended = 0): Extended; virtual; abstract;
    { Stores an Extended value. }
    procedure WriteFloat(const Path: string; Value: Extended); virtual; abstract;
    { Retrieves the specified string value. If the value is not found, the Default will be
      returned. If the value is not a string (or can't be converted to a string an EConvertError
      exception will be raised. }
    function ReadString(const Path: string; Default: string = ''): string; virtual; abstract;
    { Stores an string value. }
    procedure WriteString(const Path: string; Value: string); virtual; abstract;
    { Retrieves the specified TDateTime value. If the value is not found, the Default will be
      returned. If the value is not a TDateTime (or can't be converted to an TDateTime an
      EConvertError exception will be raised. }
    function ReadDateTime(const Path: string; Default: TDateTime = 0): TDateTime; virtual;
    { Stores a TDateTime value. }
    procedure WriteDateTime(const Path: string; Value: TDateTime); virtual;
    { Retrieves the specified value into a buffer. The result holds the number of bytes actually
      retrieved. }
    function ReadBinary(const Path: string; var Buf; BufSize: Integer): Integer; virtual; abstract;
    { Stores a buffer. }
    procedure WriteBinary(const Path: string; const Buf; BufSize: Integer); virtual; abstract;
    { Retrieves the specified list. Caller provides a callback method that will read the individual
      items. ReadList will first determine the number of items to read and calls the specified
      method for each item. }
    function ReadList(const Path: string; const OnReadItem: TAppStoreListItem): Integer; virtual;
    { Stores a list of items. The number of items is stored first. For each item the provided
      item write method is called. Any additional items in the list (from a previous write) will be
      removed by the optionally provided delete method. }
    procedure WriteList(const Path: string; const ItemCount: Integer; const OnWriteItem: TAppStoreListItem; const OnDeleteItems: TAppStoreListDelete = nil); virtual;
    { Retrieves a string list. The string list is optionally cleared before reading starts. The
      result value is the number of items read. Uses ReadList with internally provided methods to
      do the actual reading. }
    function ReadStringList(const Path: string; const SL: TStrings; const ClearFirst: Boolean = True): Integer; virtual;
    { Stores a string list. Uses WriteList with internally provided methods to do the actual
      storing. }
    procedure WriteStringList(const Path: string; const SL: TStrings); virtual;
  end;

implementation

uses
  SysUtils,
  JclStrings;

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
      While J > 0 do
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

//===TJvCustomAppStore==============================================================================

function TJvCustomAppStore.GetAppRoot: string;
begin
  Result := FAppRoot;
end;

procedure TJvCustomAppStore.SetAppRoot(Value: string);
begin
  FAppRoot := OptimizePaths([Value]);
end;

function TJvCustomAppStore.GetCurrentPath: string;
begin
  Result := GetAbsPath('');
end;

function TJvCustomAppStore.GetAbsPath(Path: string): string;
begin
  Result := GetAppRoot + '\' + OptimizePaths([GetRoot, Path]);
end;

procedure TJvCustomAppStore.ReadSLItem(Sender: TJvCustomAppStore; const Path: string; const Index: Integer);
begin
  Sender.FStoreSL.Add(Sender.ReadString(Path + '\Item' + IntToStr(Index)));
end;

procedure TJvCustomAppStore.WriteSLItem(Sender: TJvCustomAppStore; const Path: string; const Index: Integer);
begin
  Sender.WriteString(Path + '\Item' + IntToStr(Index), Sender.FStoreSL[Index]);
end;

procedure TJvCustomAppStore.DeleteSLItems(Sender: TJvCustomAppStore; const Path: string; const First, Last: Integer);
var
  I: Integer;
begin
  for I := First to Last do
    Sender.DeleteValue(Path + '\Item' + IntToStr(I));
end;

function TJvCustomAppStore.GetRoot: string;
begin
  Result := FCurRoot;
end;

procedure TJvCustomAppStore.SetRoot(const Path: string);
begin
  FCurRoot := OptimizePaths([Path]);
end;

function TJvCustomAppStore.ReadDateTime(const Path: string; Default: TDateTime): TDateTime;
begin
  Result := ReadFloat(Path, Default);
end;

procedure TJvCustomAppStore.WriteDateTime(const Path: string; Value: TDateTime);
begin
  WriteFloat(Path, Value);
end;

function TJvCustomAppStore.ReadList(const Path: string; const OnReadItem: TAppStoreListItem): Integer;
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
  Result := ReadList(Path, ReadSLItem);
end;

procedure TJvCustomAppStore.WriteStringList(const Path: string; const SL: TStrings);
begin
  FStoreSL := SL;
  WriteList(Path, SL.Count, WriteSLItem, DeleteSLItems);
end;

end.
