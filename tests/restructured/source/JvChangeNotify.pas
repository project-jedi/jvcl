{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChangeNotify.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

  {A wrapper for the Find[First/Next]ChangeNotification API calls. }

unit JvChangeNotify;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, JvComponent;

type
  TJvNotifyArray = array[0..MAXIMUM_WAIT_OBJECTS - 1] of THandle;
  TJvChangeAction = (caChangeFileName, caChangeDirName, caChangeAttributes, caChangeSize,
    caChangeLastWrite, caChangeSecurity);
  TJvChangeActions = set of TJvChangeAction;
  TJvNotifyEvent = procedure(Sender: TObject; Dir: string; Actions: TJvChangeActions) of object;
  TJvThreadNotifyEvent = procedure(Sender: TObject; Index: integer) of object;
  TJvNotifyError = procedure(Sender: TObject; const MSg: string) of object;

  TJvChangeItems = class;
  TJvChangeNotify = class;

  TJvChangeItem = class(TCollectionItem)
  private
    FParent: TJvChangeItems;
    FActions: TJvChangeActions;
    FSubTrees: boolean;
    FDir: string;
    FOnChange: TNotifyEvent;
    procedure SetSubTrees(Value: boolean);
    procedure SetDir(Value: string);
  protected
    function GetDisplayName: string; override;
    procedure Change; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Directory: string read FDir write SetDir;
    property Actions: TJvChangeActions read FActions write FActions default [caChangeFileName, caChangeDirName];
    property IncludeSubTrees: boolean read FSubTrees write SetSubTrees default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvChangeItems = class(TCollection)
  protected
    FOwner: TJvChangeNotify;
    function GetItem(Index: Integer): TJvChangeItem;
    procedure SetItem(Index: Integer; Value: TJvChangeItem);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TJvChangeNotify);
    function Add: TJvChangeItem;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TJvChangeItem read GetItem write SetItem; default;
  end;

  TJvChangeThread = class(TThread)
  private
    FNotifyArray: TJvNotifyArray;
    FCount, FIndex, FInterval: integer;
    FNotify: TJvThreadNotifyEvent;
    procedure Change(Index: integer);
    procedure SynchChange;
  public
    constructor Create(NotifyArray: TJvNotifyArray; Count, Interval: integer);
    procedure Execute; override;
    property OnChangeNotify: TJvThreadNotifyEvent read FNotify write FNotify;
  end;

  TJvChangeNotify = class(TJvComponent)
  private
    { Private declarations }
    FThread: TJvChangeThread;
    FActive: boolean;
    FInterval: integer;
    FCollection: TJvChangeItems;
    FNotify: TJvNotifyEvent;
    FNotifyArray: TJvNotifyArray;
    procedure SetCollection(Value: TJvChangeItems);
    procedure SetInterval(Value: integer);
    procedure SetActive(Value: boolean);
    procedure CheckActive;
    function NotifyError(Msg: string): string;
    procedure DoThreadChangeNotify(Sender: TObject; Index: integer);
  protected
    { Protected declarations }
    procedure Change(Item: TJvChangeItem); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Active: boolean read FActive write SetActive default false;
  published
    { Published declarations }
    property Notifications: TJvChangeItems read FCollection write SetCollection;
    property CheckInterval: integer read FInterval write SetInterval default 100;
    property OnChangeNotify: TJvNotifyEvent read FNotify write FNotify;
  end;

function ActionsToString(Actions: TJvChangeActions): string;

implementation
uses
  JvTypes
{$IFNDEF COMPILER6_UP}
  ,FileCtrl
{$ENDIF}
  ;


function ActionsToString(Actions: TJvChangeActions): string;
const
  aArray: array[TJvChangeAction] of shortstring = ('Filename Change', 'Directory Name Change', 'Attributes Change', 'Size Change',
    'Write Change', 'Security Change');
var i: TJvChangeAction;
begin
  Result := '';
  for i := Low(TJvChangeAction) to High(TJvChangeAction) do // Iterate
    if i in Actions then
      Result := Result + aArray[i] + ',';
  if Length(Result) > 0 then
    Result := Copy(Result, 1, Length(Result) - 1);
end;

{ TJvChangeItem }

constructor TJvChangeItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent := TJvChangeItems(Collection);
  FSubTrees := false;
  FActions := [caChangeFileName, caChangeDirName];
end;

destructor TJvChangeItem.Destroy;
begin
  inherited Destroy;
end;

procedure TJvChangeItem.Assign(Source: TPersistent);
begin
  if Source is TJvChangeItem then
  begin
    Directory := TJvChangeItem(Source).Directory;
    Actions := TJvChangeItem(Source).Actions;
    IncludeSubTrees := TJvChangeItem(Source).IncludeSubTrees;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TJvChangeItem.SetSubTrees(Value: boolean);
begin
  if (FSubTrees <> Value) then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      FSubTrees := Value
    else if Value then
    begin
      Assert(FParent <> nil,'FParent is nil in TJvChangeItem'); 
      Assert(FParent.FOwner <> nil,'FParent.FOWner is nil in TJvChangeItem');
      if not (csDesigning in FParent.FOwner.ComponentState) then
        raise EJVCLException.Create('This option only available on Windows NT');
    end;
  end;
end;

procedure TJvChangeItem.SetDir(Value: string);
begin
  if (Length(Value) = 0) or not DirectoryExists(Value) then
    raise EJVCLException.CreateFmt('Invalid or empty path [%s]', [Value]);
  FDir := Value;
end;

function TJvChangeItem.GetDisplayName: string;
begin
  if FDir <> '' then
    Result := FDir
  else
    Result := inherited GetDisplayName;
end;

procedure TJvChangeItem.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

{ TJvChangeItems }

constructor TJvChangeItems.Create(AOwner: TJvChangeNotify);
begin
  inherited Create(TJvChangeItem);
  FOwner := AOwner;
end;

function TJvChangeItems.Add: TJvChangeItem;
begin
  if Count < MAXIMUM_WAIT_OBJECTS then
    Result := TJvChangeItem(inherited Add)
  else
    raise EJVCLException.CreateFmt('Maximum of %d items allowed', [MAXIMUM_WAIT_OBJECTS]);
end;

function TJvChangeItems.GetItem(Index: Integer): TJvChangeItem;
begin
  Result := TJvChangeItem(inherited GetItem(Index));
end;

procedure TJvChangeItems.SetItem(Index: Integer; Value: TJvChangeItem);
begin
  inherited SetItem(Index, Value);
end;

function TJvChangeItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJvChangeItems.Assign(Source: TPersistent);
var i: integer;
begin
  if Source is TJvChangeItems then
  begin
    Clear;
    for i := 0 to TJvChangeItems(Source).Count - 1 do
      Add.Assign(TJvChangeItems(Source)[i]);
    Exit;
  end;
  inherited;
end;

{ TJvChangeNotify }

constructor TJvChangeNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCollection := TJvChangeItems.Create(self);
  FActive := false;
  FInterval := 100;
end;

destructor TJvChangeNotify.Destroy;
begin
  Active := false;
  inherited Destroy;
end;

procedure TJvChangeNotify.CheckActive;
begin
  if Active then
    raise EJVCLException.Create('Cannot change this value when active');
end;

procedure TJvChangeNotify.SetCollection(Value: TJvChangeItems);
begin
  FCollection.Assign(Value);
end;

procedure TJvChangeNotify.Change(Item: TJvChangeItem);
begin
  if Assigned(Item) then
    Item.Change
  else
    Exit;
  if Assigned(FNotify) then
    FNotify(self, Item.Directory, Item.Actions);
end;

procedure TJvChangeNotify.SetInterval(Value: integer);
begin
  CheckActive;
  if Value <= 0 then
    Exit;
  if FInterval <> Value then
    FInterval := Value;
end;

function TJvChangeNotify.NotifyError(Msg: string): string;
begin
  SetLength(Result, 256);
  SetLength(Result, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    GetLastError, 0, PChar(Result), Length(Result), nil));
  raise
    EJVCLException.CreateFmt('%s [%s]', [Result, Msg]);
end;

procedure TJvChangeNotify.DoThreadChangeNotify(Sender: TObject; Index: integer);
begin
  if Assigned(FNotify) then
    FNotify(self, Notifications[Index].Directory, Notifications[Index].Actions);
end;

procedure TJvChangeNotify.SetActive(Value: boolean);
const
  cActions: array[TJvChangeAction] of Cardinal = (FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME,
    FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE, FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_SECURITY);
var cA: TJvChangeAction; Flags: Cardinal; i: integer; S: string;
begin
  if (csDesigning in ComponentState) or (FActive = Value) then
    Exit;

  FActive := Value;
  if FActive then
  begin
    FillChar(FNotifyArray, sizeof(TJvNotifyArray), INVALID_HANDLE_VALUE);
    for i := 0 to FCollection.Count - 1 do
    begin
      Flags := 0;
      { convert TJvChangeActions to bitfields }
      for cA := Low(TJvChangeAction) to High(TJvChangeAction) do
        if cA in FCollection[i].Actions then
          Flags := Flags or (cActions[cA]);
      S := FCollection[i].Directory;
      if (Length(S) = 0) or not DirectoryExists(S) then
        raise EJVCLException.CreateFmt('Invalid or empty path ("%s") at index %d', [S, i]);
      FNotifyArray[i] := FindFirstChangeNotification(PChar(S),
        longbool(FCollection[i].IncludeSubTrees), Flags);
      if FNotifyArray[i] = INVALID_HANDLE_VALUE then
        NotifyError(FCollection[i].Directory);
    end;
    // use thread instead
    if FThread <> nil then
    begin
      FThread.Terminate;
      FreeAndNil(FThread);
    end;
    FThread := TJvChangeThread.Create(FNotifyArray, FCollection.Count, FInterval);
    FThread.OnChangeNotify := DoThreadChangeNotify;
    FThread.Resume;
  end
  else if FThread <> nil then
  begin
    FThread.Terminate;
    FreeAndNil(FThread);
  end;

  {
    while FActive do
    begin
      i := WaitForMultipleObjects(FCollection.Count, @FNotifyArray, False, FInterval);
      if (i >= 0) and (i < FCollection.Count) then
      begin
        try
          Change(FCollection.Items[i]);
        finally
          Assert(FindNextChangeNotification(FNotifyArray[i]));
        end;
      end
      else
        Application.ProcessMessages;
    end;
  for i := 0 to FCollection.Count - 1 do // Iterate
    FindCloseChangeNotification(FNotifyArray[i]);
    }
end;

{ TJvChangeThread }

constructor TJvChangeThread.Create(NotifyArray: TJvNotifyArray; Count, Interval: integer);
var i: integer;
begin
  inherited Create(true);
  FCount := Count;
  FInterval := Interval;
  FillChar(FNotifyArray, sizeof(TjvNotifyArray), INVALID_HANDLE_VALUE);
  for i := 0 to FCount - 1 do
    FNotifyArray[i] := NotifyArray[i];
end;

procedure TJvChangeThread.Change(Index: integer);
begin
  FIndex := Index;
  Synchronize(SynchChange);
end;

procedure TJvChangeThread.Execute;
var i: integer;
begin
  while not Terminated do
  begin
    i := WaitForMultipleObjects(FCount, @FNotifyArray, False, FInterval);
    if (i >= 0) and (i < FCount) then
    begin
      try
        Change(i);
      finally
        Assert(FindNextChangeNotification(FNotifyArray[i]));
      end;
    end;
  end;
  if Terminated then
    for i := 0 to FCount - 1 do // Iterate
      if FNotifyArray[i] <> INVALID_HANDLE_VALUE then
      begin
        FindCloseChangeNotification(FNotifyArray[i]);
        FNotifyArray[i] := INVALID_HANDLE_VALUE;
      end;
end;

procedure TJvChangeThread.SynchChange;
begin
  if Assigned(FNotify) then
    FNotify(self, FIndex);
end;

end.

