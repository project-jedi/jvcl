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

Contributor(s): Ralf Steinhaeusser [ralfiii@gmx.net].

Last Modified: 2002-09-02

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

uses Forms, Classes, Graphics;

type
  TPluginMessageEvent = procedure(Sender: TObject; APluginMessage: longint; AMessageText: string) of object;
  TPluginInitializeEvent = procedure(Sender: TObject; var AllowLoad: boolean) of object;
  TJvPluginCommand = class;
  TJvPluginCommands = class;

  TJvPlugin = class(TDataModule)
  private
    { Private declarations }
    fPluginID: string;
    fAuthor: string;
    fCopyright: string;
    fDescription: string;
    fFilename: string;
    fCommands: TJvPluginCommands;

    fHostApplication: TApplication;
    fManager: TComponent;
    fInstanceCount: integer;

    fOnPluginMessage: TPluginMessageEvent;
    fOnInitialize: TPluginInitializeEvent;
    fOnConfigure: TNotifyEvent;
    fPluginVersion: string;
//    function GetVersion: string;
//    procedure SetVersion(newValue: string);
  protected
    { Protected declarations }
    procedure SetCommands(newValue: TJvPluginCommands); virtual;
    procedure TriggerPluginMessageEvent(APluginMessage: longint; AMessageText: string); virtual;
    procedure TriggerInitializeEvent(var AllowLoad: boolean); virtual;
    procedure TriggerConfigureEvent; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Configure; virtual; stdcall;
    function Initialize(Manager: TComponent; HostApplication: TApplication; Filename: string): Boolean; virtual; stdcall;
    procedure SendPluginMessage(APluginMessage: longint; AMessageText: string);
    property HostApplication: TApplication read fHostApplication;
    property Manager: TComponent read fManager;
    property Filename: string read fFilename;
  published
    { Published properties and events }
    property Author: string read fAuthor write fAuthor;
    property Commands: TJvPluginCommands read fCommands write SetCommands;
    property Description: string read fDescription write fDescription;
    property Copyright: string read fCopyright write fCopyright;
    property InstanceCount: integer read fInstanceCount write fInstanceCount default 1;
    property PluginID: string read fPluginID write fPluginID;
//    property Version: string read GetVersion write SetVersion;
    property PluginVersion: string read fPluginVersion write fPluginVersion;
    property OnPluginMessage: TPluginMessageEvent read fOnPluginMessage write fOnPluginMessage;
    property OnInitialize: TPluginInitializeEvent read fOnInitialize write fOnInitialize;
    property OnConfigure: TNotifyEvent read fOnConfigure write fOnConfigure;
  end; { TJvPlugin }

  TJvPluginCommand = class(TCollectionItem)
  private
    fName: string;
    fCaption: string;
    fHint: string;
    fData: string;
    fBitmap: TBitmap;
    fOnExecute: TNotifyEvent;
    procedure SetBitmap(Value: TBitmap);
  protected
    function GetDisplayName: string; override;
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Bitmap: TBitmap read fBitmap write SetBitmap;
    property Caption: string read fCaption write fCaption;
    property Hint: string read fHint write fHint;
    property Data: string read fData write fData;
    property Name: string read fName write fName;
    property OnExecute: TNotifyEvent read fOnExecute write fOnExecute;
  end; // TJvPluginCommand

  TJvPluginCommands = class(TCollection)
  private
    fPlugin: TJvPlugin;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(APlugin: TJvPlugin);
  end; // TJvPluginCommands

implementation

uses JvPlugCommon;

// ###################################
// ######  Create, Free...
// ###################################

constructor TJvPlugin.Create(AOwner: TComponent);
{ Creates an object of type TJvPlugin, and initializes properties. }
begin
  {$IFNDEF COMPILER3}
  GlobalNameSpace.BeginWrite;
  {$ENDIF}
  try
    // Create datamodule
    CreateNew(AOwner);
    DesignSize := Point(100, 100);

    // Create commands-collection
    fCommands := TJvPluginCommands.Create(Self);

    fInstanceCount := 1;
    if (ClassType <> TJvPlugin) and not (csDesigning in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TJvPlugin) then
        raise EResNotFound.CreateFmt('Resource Not Found: %s', [ClassName]);

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

// ###################################
// ######   Setting of properties
// ###################################

procedure TJvPlugin.SetCommands(newValue: TJvPluginCommands);
// Sets data member fCommands to newValue.
begin
  fCommands.Assign(newValue);
end;

// Show Versionsstring defined in JvPlugCommon

{function TJvPlugin.GetVersion: string;
begin
  result := C_VersionString;
end;

procedure TJvPlugin.SetVersion(newValue: string);
begin
end;}

// ###################################
// ######
// ###################################

// Here the plugin should verify if it CAN be loaded (e.g. Mainapplication implements correct interface,
//      Dongle is there....)

function TJvPlugin.Initialize(Manager: TComponent; HostApplication: TApplication; Filename: string): Boolean;
begin
  Result := true;
  fHostApplication := HostApplication;
  fFilename := Filename;
  fManager := Manager;
  TriggerInitializeEvent(Result);
end;

procedure TJvPlugin.Configure;
begin
  TriggerConfigureEvent;
end;

procedure TJvPlugin.TriggerPluginMessageEvent(APluginMessage: longint; AMessageText: string);
begin
  if (assigned(fOnPluginMessage)) then
    fOnPluginMessage(Self, APluginMessage, AMessageText);
end;

procedure TJvPlugin.TriggerInitializeEvent(var AllowLoad: boolean);
begin
  if (assigned(fOnInitialize)) then
    fOnInitialize(Self, AllowLoad);
end;

procedure TJvPlugin.TriggerConfigureEvent;
begin
  if (assigned(fOnConfigure)) then
    fOnConfigure(Self);
end;

procedure TJvPlugin.SendPluginMessage(APluginMessage: Integer; AMessageText: string);
begin
  TriggerPluginMessageEvent(APluginMessage, AMessageText);
end;

// ###################################
// ###################################
// ######   TJvPluginCommand
// ###################################
// ###################################

constructor TJvPluginCommand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fBitmap := TBitmap.Create;
end;

destructor TJvPluginCommand.Destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;

// ###################################

function TJvPluginCommand.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TJvPluginCommand.SetBitmap(Value: TBitmap);
begin
  fBitmap.Assign(Value);
end;

// ###################################
// ######   TJvPluginCommands
// ###################################

constructor TJvPluginCommands.Create(aPlugin: TJvPlugin);
begin
  inherited Create(TJvPluginCommand);
  fPlugin := aPlugin;
end;

// ###################################

function TJvPluginCommands.GetOwner: TPersistent;
begin
  Result := fPlugin;
end;

end.

