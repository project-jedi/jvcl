{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uilPlugin.PAS, released on 1999-09-06.

The Initial Developer of the Original Code is Tim Sullivan [tim@uil.net]
Portions created by Tim Sullivan are Copyright (C) 1999 Tim Sullivan.
All Rights Reserved.

Contributor(s):
Ralf Steinhaeusser [ralfiii@gmx.net].
Gustavo Bianconi

Last Modified: 2003-05-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Todo : Why the "stdcall" definitions ? (routines Configure, Initialize...)
       Why the TriggerConfigureEvent (and similar) procedures ? necessary ?
       What for the GlobalNameSpace.BeginWrite ?
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPlugin;

interface

uses
  Forms, Classes, Graphics;

type
  TPluginMessageEvent = procedure(Sender: TObject; APluginMessage: Longint; AMessageText: string) of object;
  TPluginInitializeEvent = procedure(Sender: TObject; var AllowLoad: Boolean) of object;
  TJvPluginCommand = class;
  TJvPluginCommands = class;

  TJvPlugin = class(TDataModule)
  private
    FPluginID: string;
    FAuthor: string;
    FCopyright: string;
    FDescription: string;
    FFilename: string;
    FCommands: TJvPluginCommands;
    FHostApplication: TApplication;
    FManager: TComponent;
    FInstanceCount: Integer;
    FOnPluginMessage: TPluginMessageEvent;
    FOnInitialize: TPluginInitializeEvent;
    FOnConfigure: TNotifyEvent;
    FPluginVersion: string;
//    function GetVersion: string;
//    procedure SetVersion(newValue: string);
  protected
    procedure SetCommands(NewValue: TJvPluginCommands); virtual;
    procedure TriggerPluginMessageEvent(APluginMessage: Longint; AMessageText: string); virtual;
    procedure TriggerInitializeEvent(var AllowLoad: Boolean); virtual;
    procedure TriggerConfigureEvent; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Configure; virtual; stdcall;
    function Initialize(Manager: TComponent; HostApplication: TApplication; Filename: string): Boolean; virtual;
      stdcall;
    procedure SendPluginMessage(APluginMessage: Longint; AMessageText: string);
    property HostApplication: TApplication read FHostApplication;
    property Manager: TComponent read FManager;
    property Filename: string read FFilename;
  published
    property Author: string read FAuthor write FAuthor;
    property Commands: TJvPluginCommands read FCommands write SetCommands;
    property Description: string read FDescription write FDescription;
    property Copyright: string read FCopyright write FCopyright;
    property InstanceCount: Integer read FInstanceCount write FInstanceCount default 1;
    property PluginID: string read FPluginID write FPluginID;
//    property Version: string read GetVersion write SetVersion;
    property PluginVersion: string read FPluginVersion write FPluginVersion;
    property OnPluginMessage: TPluginMessageEvent read FOnPluginMessage write FOnPluginMessage;
    property OnInitialize: TPluginInitializeEvent read FOnInitialize write FOnInitialize;
    property OnConfigure: TNotifyEvent read FOnConfigure write FOnConfigure;
  end;

  TJvPluginCommand = class(TCollectionItem)
  private
    FName: string;
    FCaption: string;
    FHint: string;
    FData: string;
    FShortCut : TShortCut;
    FBitmap: TBitmap;
    FOnExecute: TNotifyEvent;
    procedure SetBitmap(Value: TBitmap);
  protected
    function GetDisplayName: string; override;
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Caption: string read FCaption write FCaption;
    property Hint: string read FHint write FHint;
    property Data: string read FData write FData;
    property Name: string read FName write FName;
    property ShortCut : TShortCut read FShortCut write FShortCut;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

  TJvPluginCommands = class(TCollection)
  private
    FPlugin: TJvPlugin;
  protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(AItem: TCollectionItem); override;
  public
    constructor Create(APlugin: TJvPlugin);
  end;

implementation

uses
  SysUtils;

//=== TJvPlugin ==============================================================

constructor TJvPlugin.Create(AOwner: TComponent);
begin
  // (rom) where is inherited Create(AOwner) ?
  {$IFNDEF COMPILER3}
  GlobalNameSpace.BeginWrite;
  {$ENDIF}
  try
    // Create datamodule
    CreateNew(AOwner);
    DesignSize := Point(100, 100);

    // Create commands-collection
    FCommands := TJvPluginCommands.Create(Self);

    FInstanceCount := 1;
    if (ClassType <> TJvPlugin) and not (csDesigning in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TJvPlugin) then
        raise EResNotFound.CreateFmt('Resource Not Found: %s', [ClassName]);

      // (rom) why this ?
      {$IFNDEF COMPILER3}
      if OldCreateOrder and Assigned(OnCreate) then
        OnCreate(Self);
      {$ELSE}
      if Assigned(OnCreate) then
        OnCreate(Self);
      {$ENDIF}
    end;
  finally
    {$IFNDEF COMPILER3}
    GlobalNameSpace.EndWrite;
    {$ENDIF}
  end;
end;

destructor TJvPlugin.Destroy;
begin
  Commands.Free;
  inherited Destroy;
end;

procedure TJvPlugin.SetCommands(NewValue: TJvPluginCommands);
begin
  FCommands.Assign(NewValue);
end;

// Show Versionsstring defined in JvPlugCommon

{function TJvPlugin.GetVersion: string;
begin
  Result := C_VersionString;
end;

procedure TJvPlugin.SetVersion(newValue: string);
begin
end;}

// Here the plugin should verify if it CAN be loaded (e.g. Mainapplication implements correct interface,
//      Dongle is there....)

function TJvPlugin.Initialize(Manager: TComponent; HostApplication: TApplication; Filename: string): Boolean;
begin
  Result := True;
  FHostApplication := HostApplication;
  FFilename := Filename;
  FManager := Manager;
  TriggerInitializeEvent(Result);
end;

procedure TJvPlugin.Configure;
begin
  TriggerConfigureEvent;
end;

procedure TJvPlugin.TriggerPluginMessageEvent(APluginMessage: Longint; AMessageText: string);
begin
  if (Assigned(FOnPluginMessage)) then
    FOnPluginMessage(Self, APluginMessage, AMessageText);
end;

procedure TJvPlugin.TriggerInitializeEvent(var AllowLoad: Boolean);
begin
  if (Assigned(FOnInitialize)) then
    FOnInitialize(Self, AllowLoad);
end;

procedure TJvPlugin.TriggerConfigureEvent;
begin
  if (Assigned(FOnConfigure)) then
    FOnConfigure(Self);
end;

procedure TJvPlugin.SendPluginMessage(APluginMessage: Integer; AMessageText: string);
begin
  TriggerPluginMessageEvent(APluginMessage, AMessageText);
end;

//=== TJvPluginCommand =======================================================

constructor TJvPluginCommand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FShortCut := 0;
end;

destructor TJvPluginCommand.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TJvPluginCommand.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TJvPluginCommand.SetBitmap(Value: TBitmap);
begin
  FreeAndNil(FBitmap);
  if Assigned(Value) then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.Assign(Value);
  end;
end;

//=== TJvPluginCommands ======================================================

constructor TJvPluginCommands.Create(APlugin: TJvPlugin);
begin
  inherited Create(TJvPluginCommand);
  FPlugin := APlugin;
end;

function TJvPluginCommands.GetOwner: TPersistent;
begin
  Result := FPlugin;
end;

procedure TJvPluginCommands.SetItemName(AItem: TCollectionItem);
var
  I: Integer;
  J: Integer;

  function NameUsed: Boolean;
  begin
    J := AItem.Collection.Count - 1;
    while (J > -1) and (TJvPluginCommand(AItem.Collection.Items[J]).Name <> ('Command' +
      IntToStr(I))) do
      Dec(J);
    Result := J > -1;
  end;

  procedure FindCmdIdx;
  begin
    I := 1;
    while (I < MaxInt) and NameUsed do
      Inc(I);
  end;

begin
  with TJvPluginCommand(AItem) do
    if Name = '' then
    begin
      FindCmdIdx;
      Name := 'Command' + IntToStr(I);
    end;
end;

end.

