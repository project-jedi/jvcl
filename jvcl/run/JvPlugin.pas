{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uilPlugin.PAS, released on 1999-09-06.

The Initial Developer of the Original Code is Tim Sullivan [tim att uil dott net]
Portions created by Tim Sullivan are Copyright (C) 1999 Tim Sullivan.
All Rights Reserved.

Contributor(s):
Ralf Steinhaeusser [ralfiii att gmx dott net].
Gustavo Bianconi
Steefan Lesage - converted to use new OTA

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

Todo : Why the "stdcall" definitions ? (routines Configure, Initialize...)
       Why the TriggerConfigureEvent (and similar) procedures ? necessary ?
       What for the GlobalNameSpace.BeginWrite ?
-----------------------------------------------------------------------------}
// $Id$

unit JvPlugin;

{$I jvcl.inc}

interface

uses
  SysUtils,
  {$IFDEF VCL}
  Forms, Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms, QGraphics,
  {$ENDIF VisualCLX}
  Classes;

type
  TPluginMessageEvent = procedure(Sender: TObject; APluginMessage: Longint; AMessageText: string) of object;
  TPluginInitializeEvent = procedure(Sender: TObject; var AllowLoad: Boolean) of object;
  TJvPluginCommand = class;
  TJvPluginCommands = class;

  TJvPlugIn = class(TDataModule)
  private
    FPluginID: string;
    FAuthor: string;
    FCopyright: string;
    FDescription: string;
    FFileName: TFileName;
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
    function Initialize(Manager: TComponent; HostApplication: TApplication;
      FileName: string): Boolean; virtual; stdcall;
    procedure SendPluginMessage(APluginMessage: Longint; AMessageText: string);
    property HostApplication: TApplication read FHostApplication;
    property Manager: TComponent read FManager;
    property FileName: TFileName read FFileName;
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
    FShortCut: TShortCut;
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
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

  TJvPluginCommands = class(TCollection)
  private
    FPlugin: TJvPlugIn;
  protected
    function GetOwner: TPersistent; override;
    procedure SetItemName(AItem: TCollectionItem); override;
  public
    constructor Create(APlugIn: TJvPlugIn);
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvResources;

//=== { TJvPlugin } ==========================================================

constructor TJvPlugIn.Create(AOwner: TComponent);
begin
  try
    // Create datamodule
    CreateNew(AOwner);
    DesignSize := Point(100, 100);

    // Create commands-collection
    FCommands := TJvPluginCommands.Create(Self);

    FInstanceCount := 1;
    if (ClassType <> TJvPlugIn) and not (csDesigning in ComponentState) then
    begin
      if not InitInheritedComponent(Self, TJvPlugIn) then
        raise EResNotFound.CreateResFmt(@RsEFmtResNotFound, [ClassName]);

      // (rom) why this ?
      if Assigned(OnCreate) then
        OnCreate(Self);
    end;
  finally
  end;
end;

destructor TJvPlugIn.Destroy;
begin
  Commands.Free;
  inherited Destroy;
end;

procedure TJvPlugIn.SetCommands(NewValue: TJvPluginCommands);
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

// Here the plugin should verify if it CAN be loaded (e.g. Main application implements correct interface,
//      Dongle is there....)

function TJvPlugIn.Initialize(Manager: TComponent; HostApplication: TApplication; FileName: string): Boolean;
begin
  Result := True;
  FHostApplication := HostApplication;
  FFileName := FileName;
  FManager := Manager;
  TriggerInitializeEvent(Result);
end;

procedure TJvPlugIn.Configure;
begin
  TriggerConfigureEvent;
end;

procedure TJvPlugIn.TriggerPluginMessageEvent(APluginMessage: Longint; AMessageText: string);
begin
  if Assigned(FOnPluginMessage) then
    FOnPluginMessage(Self, APluginMessage, AMessageText);
end;

procedure TJvPlugIn.TriggerInitializeEvent(var AllowLoad: Boolean);
begin
  if Assigned(FOnInitialize) then
    FOnInitialize(Self, AllowLoad);
end;

procedure TJvPlugIn.TriggerConfigureEvent;
begin
  if Assigned(FOnConfigure) then
    FOnConfigure(Self);
end;

procedure TJvPlugIn.SendPluginMessage(APluginMessage: Integer; AMessageText: string);
begin
  TriggerPluginMessageEvent(APluginMessage, AMessageText);
end;

//=== { TJvPluginCommand } ===================================================

constructor TJvPluginCommand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBitmap := TBitmap.Create;
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
  FBitmap.Assign(Value)
end;

//=== { TJvPluginCommands } ==================================================

constructor TJvPluginCommands.Create(APlugIn: TJvPlugIn);
begin
  inherited Create(TJvPluginCommand);
  FPlugin := APlugIn;
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
  var
    AName: string;
  begin
    AName := Format('Command%d', [I]);
    J := AItem.Collection.Count - 1;
    while (J > -1) and not AnsiSameText(TJvPluginCommand(AItem.Collection.Items[J]).Name, AName) do
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
      Name := Format('Command%d', [I]);
    end;
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

