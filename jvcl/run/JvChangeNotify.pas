{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChangeNotify.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  A wrapper for the Find[First/Next]ChangeNotification API calls.

Changes:
  //dierk schmid 2004-4-28
  -- TJvChangeNotify: Put property "active" from public to published section
     (cause I always forget to set this property in runtime to true)
  -- TJvChangeNotify.SetActive: Exit if csDesigning in ComponentState (Active is now published)
  -- TJvChangeItem.SetDir: Exception not when csDesigning in ComponentState
     (cause, it was impossible to reset in designtime the directory property)
  -- Same TJvChangeNotify.CheckActive: Exception not when csDesigning+csloading in ComponentState
  -- added procedure TJvChangeNotify.Loaded; override;


Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvChangeNotify;

interface

uses
  Windows, Classes,
  JvComponent;

type
  TJvNotifyArray = array [0..MAXIMUM_WAIT_OBJECTS - 1] of THandle;
  TJvChangeAction = (caChangeFileName, caChangeDirName, caChangeAttributes, caChangeSize,
    caChangeLastWrite, caChangeSecurity);
  TJvChangeActions = set of TJvChangeAction;
  TJvNotifyEvent = procedure(Sender: TObject; Dir: string; Actions: TJvChangeActions) of object;
  TJvThreadNotifyEvent = procedure(Sender: TObject; Index: Integer) of object;
  TJvNotifyError = procedure(Sender: TObject; const Msg: string) of object;

  TJvChangeItems = class;
  TJvChangeNotify = class;

  TJvChangeItem = class(TCollectionItem)
  private
    FParent: TJvChangeItems;
    FActions: TJvChangeActions;
    FSubTrees: Boolean;
    FDir: string;
    FOnChange: TNotifyEvent;
    procedure SetSubTrees(const Value: Boolean);
    procedure SetDir(const Value: string);
  protected
    function GetDisplayName: string; override;
    procedure Change; virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Directory: string read FDir write SetDir;
    property Actions: TJvChangeActions read FActions write FActions default [caChangeFileName, caChangeDirName];
    property IncludeSubTrees: Boolean read FSubTrees write SetSubTrees default False;
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
    FCount: Integer;
    FIndex: Integer;
    FInterval: Integer;
    FNotify: TJvThreadNotifyEvent;
    procedure SynchChange;
  public
    constructor Create(NotifyArray: TJvNotifyArray; Count, Interval: Integer);
    procedure Execute; override;
    property OnChangeNotify: TJvThreadNotifyEvent read FNotify write FNotify;
  end;

  TJvChangeNotify = class(TJvComponent)
  private
    FThread: TJvChangeThread;
    FActive: Boolean;
    FInterval: Integer;
    FCollection: TJvChangeItems;
    FNotify: TJvNotifyEvent;
    FNotifyArray: TJvNotifyArray;
    procedure SetCollection(const Value: TJvChangeItems);
    procedure SetInterval(const Value: Integer);
    procedure SetActive(const Value: Boolean);
    procedure CheckActive(const Name: string);
    function NotifyError(const Msg: string): string;
    procedure DoThreadChangeNotify(Sender: TObject; Index: Integer);
    procedure DoThreadTerminate(Sender: TObject);
  protected
    procedure Change(Item: TJvChangeItem); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Notifications: TJvChangeItems read FCollection write SetCollection;
    property CheckInterval: Integer read FInterval write SetInterval default 100;
    property OnChangeNotify: TJvNotifyEvent read FNotify write FNotify;
  end;

function ActionsToString(Actions: TJvChangeActions): string;

implementation

uses
  SysUtils, 
  JvJCLUtils, JvResources, JvTypes;
  // JvJCLUtils for DirectoryExists

function ActionsToString(Actions: TJvChangeActions): string;
const
  ActionStrings: array [TJvChangeAction] of string =
    (RsFileNameChange, RsDirectoryNameChange, RsAttributesChange,
     RsSizeChange, RsWriteChange, RsSecurityChange);
var
  I: TJvChangeAction;
begin
  Result := '';
  for I := Low(TJvChangeAction) to High(TJvChangeAction) do
    if I in Actions then
      if Result = '' then
        Result := ActionStrings[I]
      else
        Result := Result + ',' + ActionStrings[I];
end;

//=== { TJvChangeItem } ======================================================

constructor TJvChangeItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent := TJvChangeItems(Collection);
  FSubTrees := False;
  FActions := [caChangeFileName, caChangeDirName];
end;

procedure TJvChangeItem.Assign(Source: TPersistent);
begin
  if Source is TJvChangeItem then
  begin
    Directory := TJvChangeItem(Source).Directory;
    Actions := TJvChangeItem(Source).Actions;
    IncludeSubTrees := TJvChangeItem(Source).IncludeSubTrees;
  end
  else
    inherited Assign(Source);
end;

procedure TJvChangeItem.SetSubTrees(const Value: Boolean);
begin
  if FSubTrees <> Value then
  begin
    if csDesigning in FParent.FOwner.ComponentState then
      FSubTrees := Value
    else
    if Value then
      FSubTrees := Value and (Win32Platform = VER_PLATFORM_WIN32_NT)
    else
      FSubTrees := False;
  end;
end;

procedure TJvChangeItem.SetDir(const Value: string);
begin
  if FDir <> Value then
  begin
    if not (csDesigning in FParent.FOwner.ComponentState) and
           ((Length(Value) = 0) or not DirectoryExists(Value)) then
      raise EJVCLException.CreateResFmt(@RsEFmtInvalidPath, [Value]);
    FDir := Value;
  end;
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
    FOnChange(Self);
end;

//=== { TJvChangeItems } =====================================================

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
    raise EJVCLException.CreateResFmt(@RsEFmtMaxCountExceeded, [MAXIMUM_WAIT_OBJECTS]);
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
var
  I: Integer;
begin
  if Source is TJvChangeItems then
  begin
    Clear;
    for I := 0 to TJvChangeItems(Source).Count - 1 do
      Add.Assign(TJvChangeItems(Source)[I]);
  end
  else
    inherited Assign(Source);
end;

//=== { TJvChangeNotify } ====================================================

constructor TJvChangeNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCollection := TJvChangeItems.Create(Self);
  FActive := False;
  FInterval := 100;
end;

destructor TJvChangeNotify.Destroy;
begin
  Active := False;
  FCollection.Free;
  inherited Destroy;
end;

procedure TJvChangeNotify.CheckActive(const Name: string);
begin
  if Active and
     not ((csDesigning in ComponentState) or (csLoading in ComponentState)) then   //active is now published
    raise EJVCLException.CreateResFmt(@RsEFmtCannotChangeName, [Name]);
end;

procedure TJvChangeNotify.SetCollection(const Value: TJvChangeItems);
begin
  FCollection.Assign(Value);
end;

procedure TJvChangeNotify.Change(Item: TJvChangeItem);
begin
  if Assigned(Item) then
  begin
    Item.Change;
    if Assigned(FNotify) then
      FNotify(Self, Item.Directory, Item.Actions);
  end;
end;

procedure TJvChangeNotify.SetInterval(const Value: Integer);
begin
  CheckActive('Interval');
  if Value <= 0 then
    Exit;
  if FInterval <> Value then
    FInterval := Value;
end;

function TJvChangeNotify.NotifyError(const Msg: string): string;
begin
  SetLength(Result, 256);
  SetLength(Result, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    GetLastError, 0, PChar(Result), Length(Result), nil));
  raise EJVCLException.CreateResFmt(@RsENotifyErrorFmt, [Result, Msg]);
end;

procedure TJvChangeNotify.DoThreadChangeNotify(Sender: TObject; Index: Integer);
begin
  Change(Notifications[Index]);
end;

procedure TJvChangeNotify.DoThreadTerminate(Sender: TObject);
begin
  FThread := nil;
end;

procedure TJvChangeNotify.SetActive(const Value: Boolean);
const
  cActions: array [TJvChangeAction] of Cardinal =
    (FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME,
     FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
     FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_SECURITY);
var
  cA: TJvChangeAction;
  Flags: Cardinal;
  I: Integer;
  S: string;
begin
  if (FActive <> Value) then
  begin
    FActive := Value;
    if (csDesigning in ComponentState) then Exit;   //active is now published

    if FActive then
    begin
      FillChar(FNotifyArray, SizeOf(TJvNotifyArray), INVALID_HANDLE_VALUE);
      for I := 0 to FCollection.Count - 1 do
      begin
        Flags := 0;
        { convert TJvChangeActions to bitfields }
        for cA := Low(TJvChangeAction) to High(TJvChangeAction) do
          if cA in FCollection[I].Actions then
            Flags := Flags or (cActions[cA]);
        S := FCollection[I].Directory;
        if (Length(S) = 0) or not DirectoryExists(S) then
          raise EJVCLException.CreateResFmt(@RsEFmtInvalidPathAtIndex, [S, I]);
        FNotifyArray[I] := FindFirstChangeNotification(PChar(S),
          BOOL(FCollection[I].IncludeSubTrees), Flags);
        if FNotifyArray[I] = INVALID_HANDLE_VALUE then
          NotifyError(FCollection[I].Directory);
      end;
      if FThread <> nil then
      begin
        FThread.Terminate;
        FThread.WaitFor;
        FreeAndNil(FThread);
      end;
      FThread := TJvChangeThread.Create(FNotifyArray, FCollection.Count, FInterval);
      FThread.OnChangeNotify := DoThreadChangeNotify;
      FThread.OnTerminate := DoThreadTerminate;
      FThread.Resume;
    end
    else
    if FThread <> nil then
    begin
      FThread.Terminate;
      FThread := nil;
  //    FThread.WaitFor;
  //    FreeAndNil(FThread);
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
end;

procedure TJvChangeNotify.Loaded;
begin
  inherited Loaded;
  if FActive then
  begin
    FActive := False;
    SetActive(True);
  end;
end;


//=== { TJvChangeThread } ====================================================

constructor TJvChangeThread.Create(NotifyArray: TJvNotifyArray; Count, Interval: Integer);
var
  I: Integer;
begin
  inherited Create(True);
  FCount := Count;
  FInterval := Interval;
  FillChar(FNotifyArray, SizeOf(TJvNotifyArray), INVALID_HANDLE_VALUE);
  for I := 0 to FCount - 1 do
    FNotifyArray[I] := NotifyArray[I];
  FreeOnTerminate := true;
end;


procedure TJvChangeThread.Execute;
var
  I: Integer;
begin
  // (rom) secure thread against exceptions (Delphi 5 needs it)
  try
    while not Terminated do
    begin
      I := WaitForMultipleObjects(FCount, @FNotifyArray[0], False, FInterval);
      if (I >= 0) and (I < FCount) then
      begin
        try
          FIndex := i;
          Synchronize(SynchChange);
        finally
          // (rom) raising an exception in a thread is not a good idea
          // (rom) Assert removed
          //Assert(FindNextChangeNotification(FNotifyArray[I]));
          FindNextChangeNotification(FNotifyArray[I]);
        end;
      end;
    end;
    if Terminated then
      for I := 0 to FCount - 1 do
        if FNotifyArray[I] <> INVALID_HANDLE_VALUE then
        begin
          FindCloseChangeNotification(FNotifyArray[I]);
          FNotifyArray[I] := INVALID_HANDLE_VALUE;
        end;
  except
  end;
end;

procedure TJvChangeThread.SynchChange;
begin
  if Assigned(FNotify) then
    FNotify(Self, FIndex);
end;


end.

