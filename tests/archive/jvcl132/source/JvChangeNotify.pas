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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

{A wrapper for the Find[First/Next]ChangeNotification API calls. }

unit JvChangeNotify;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, FileCtrl, JvComponent;

type
  TJvNotifyArray = array [0..MAXIMUM_WAIT_OBJECTS - 1] of integer;
  TJvChangeAction=(caChangeFileName,caChangeDirName,caChangeAttributes,caChangeSize,
                 caChangeLastWrite,caChangeSecurity);
  TJvChangeActions=set of TJvChangeAction;
  TJvNotifyEvent=procedure(Sender:TObject;Dir:string;Actions:TJvChangeActions) of object;

  TJvChangeItems=class;
  TJvChangeNotify=class;


  TJvChangeItem=class(TCollectionItem)
  private
    FParent:TJvChangeItems;
    FActions:TJvChangeActions;
    FSubTrees:boolean;
    FDir:string;
    FChange:TNotifyEvent;
    procedure SetSubTrees(Value:boolean);
    procedure SetDir(Value:string);
  protected
    function GetDisplayName: string; override;
    procedure Change;virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy;override;
    procedure Assign(Source: TPersistent); override;
  published
    property Directory:string read FDir write SetDir;
    property Actions:TJvChangeActions read FActions write FActions default [caChangeFileName,caChangeDirName];
    property IncludeSubTrees:boolean read FSubTrees write SetSubTrees default false;
    property OnChange:TNotifyEvent read FChange write FChange;
  end;

  TJvChangeItems=class(TCollection)
  protected
    FOwner:TJvChangeNotify;
    function GetItem(Index: Integer): TJvChangeItem;
    procedure SetItem(Index: Integer; Value: TJvChangeItem);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner:TJvChangeNotify);
    function Add: TJvChangeItem;
    property Items[Index: Integer]: TJvChangeItem read GetItem write SetItem; default;
  end;

  TJvChangeNotify = class(TJvComponent)
  private
    { Private declarations }
    FActive:boolean;
    FInterval:integer;
    FCollection:TJvChangeItems;
    FNotify:TJvNotifyEvent;
    FNotifyArray:array [0..MAXIMUM_WAIT_OBJECTS - 1] of THandle;
    procedure SetCollection(Value:TJvChangeItems);
    procedure SetInterval(Value:integer);
    procedure SetActive(Value:boolean);
    procedure CheckActive;
    function NotifyError(Msg:string):string;
  protected
    { Protected declarations }
    procedure Change(Item:TJvChangeItem);virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property Active:boolean read FActive write SetActive default false;
  published
    { Published declarations }
    property Notifications:TJvChangeItems read FCollection write SetCollection;
    property CheckInterval:integer read FInterval write SetInterval default 100;
    property OnChangeNotify:TJvNotifyEvent read FNotify write FNotify;
  end;


function ActionsToString(Actions:TJvChangeActions):string;

implementation

function ActionsToString(Actions:TJvChangeActions):string;
const
  aArray:array[TJvChangeAction] of shortstring=('Filename Change','Directory Name Change','Attributes Change','Size Change',
                 'Write Change','Security Change');
var i:TJvChangeAction;
begin
  Result := '';
  for i := Low(TJvChangeAction) to High(TJvChangeAction) do    // Iterate
    if i in Actions then
      Result := Result + aArray[i] + ',';
  if Length(Result) > 0 then
    Result := Copy(Result,1,Length(Result) - 1);
end;


{ TJvChangeItem }

constructor TJvChangeItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent := TJvChangeItems(Collection);
  FSubTrees := false;
  FActions := [caChangeFileName,caChangeDirName];
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

procedure TJvChangeItem.SetSubTrees(Value:boolean);
begin
  if (FSubTrees <> Value) then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      FSubTrees := Value
    else
      raise Exception.Create('This option only available on Windows NT');
  end;
end;

procedure TJvChangeItem.SetDir(Value:string);
begin
  if (Length(Value) = 0) or not DirectoryExists(Value) then
    raise Exception.CreateFmt('Invalid or empty path [%s]',[Value]);
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
  if Assigned(FChange) then FChange(self);
end;


{ TJvChangeItems }

constructor TJvChangeItems.Create(AOwner:TJvChangeNotify);
begin
  inherited Create(TJvChangeItem);
  FOwner := AOwner;
end;

function TJvChangeItems.Add: TJvChangeItem;
begin
   Result := TJvChangeItem(inherited Add);
end;

function TJvChangeItems.GetItem(Index: Integer): TJvChangeItem;
begin
  Result := TJvChangeItem(inherited GetItem(Index));
end;

procedure TJvChangeItems.SetItem(Index: Integer; Value: TJvChangeItem);
begin
  inherited SetItem(Index,Value);
end;

function TJvChangeItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TJvChangeNotify }

constructor TJvChangeNotify.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FCollection := TJvChangeItems.Create(self);
  FActive := false;
  FInterval := 100;
end;

destructor TJvChangeNotify.Destroy;
begin
  if FActive then
  begin
    FActive := false;
    Application.ProcessMessages;
  end;
  inherited Destroy;
end;

procedure TJvChangeNotify.CheckActive;
begin
  if FActive then
  raise Exception.Create('Cannot change this value when active');
end;

procedure TJvChangeNotify.SetCollection(Value:TJvChangeItems);
begin
  FCollection.Assign(Value);
end;

procedure TJvChangeNotify.Change(Item:TJvChangeItem);
begin
  if Assigned(Item) then Item.Change else Exit;
  if Assigned(FNotify) then FNotify(self,Item.Directory,Item.Actions);
end;

procedure TJvChangeNotify.SetInterval(Value:integer);
begin
  CheckActive;
  if Value <= 0 then Exit;
  if FInterval <> Value then
    FInterval := Value;
end;

function TJvChangeNotify.NotifyError(Msg:string):string;
begin
  SetLength(Result,256);
  SetLength(Result,FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,
                                   GetLastError,0,PChar(Result),Length(Result),nil));
  raise
    Exception.CreateFmt('%s [%s]',[Result,Msg]);
end;

procedure TJvChangeNotify.SetActive(Value:boolean);
const
  cActions:array[TJvChangeAction] of integer = (FILE_NOTIFY_CHANGE_FILE_NAME,FILE_NOTIFY_CHANGE_DIR_NAME,
            FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,FILE_NOTIFY_CHANGE_LAST_WRITE,FILE_NOTIFY_CHANGE_SECURITY);
var cA:TJvChangeAction;Flags,i:integer;S:string;
begin
  if (csDesigning in ComponentState) or (FActive = Value) then
    Exit;

  FActive := Value;
  if not FActive then Exit;
  FillChar(FNotifyArray,sizeof(TJvNotifyArray),INVALID_HANDLE_VALUE);
  for i := 0 to FCollection.Count - 1 do
  begin
    Flags := 0;
    { convert TJvChangeActions to bitfields }
    for cA := Low(TJvChangeAction) to High(TJvChangeAction) do
      if cA in FCollection[i].Actions then  Flags := Flags or (cActions[cA]);
    S := FCollection[i].Directory;
    if (Length(S) = 0) or not DirectoryExists(S) then
      raise Exception.CreateFmt('Invalid or empty path ("%s") at index %d',[S,i]);
    FNotifyArray[i] := FindFirstChangeNotification(PChar(S),
                      longbool(FCollection[i].IncludeSubTrees),Flags);
    if FNotifyArray[i] = INVALID_HANDLE_VALUE then
       NotifyError(FCollection[i].Directory);
  end;

  while FActive do
  begin
    i := WaitForMultipleObjects(FCollection.Count,@FNotifyArray,False,FInterval);
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
  for i := 0 to FCollection.Count - 1 do    // Iterate
    FindCloseChangeNotification(FNotifyArray[i]);
end;

end.
