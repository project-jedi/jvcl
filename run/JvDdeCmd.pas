{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppDdeCmd.PAS, released Jan 2, 1998.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDdeCmd;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Messages, DdeMan, Classes,
  JclBase,
  JvComponent;

type
  EJvADCParserError = class(EJclError);

  TJvADCBusyEvent = procedure(Sender: TObject; IsBusy: Boolean) of object;

  TJvADCParsedEvent = procedure(Sender: TObject; const Command: string;
    Parameters: TStrings) of object;

  TJvADCMacroEvent = procedure(Sender: TObject; const CommandStr: string) of object;

  TJvAppDdeCmd = class(TJvComponent)
  private
    FCorrectParams: Boolean;
    FEnabled: Boolean;
    FCommands: TStringList;
    FIgnoreAppBusy: Boolean;
    FModalState: Boolean;
    FOnBusyChanged: TJvADCBusyEvent;
    FOnExecCommand: TJvADCMacroEvent;
    FOnExecParsedCmd: TJvADCParsedEvent;
    function GetCommands: TStrings;
    procedure SetEnabled(Value: Boolean);
    procedure SetIgnoreAppBusy(Value: Boolean);
    procedure SetModalState(Value: Boolean);
    function AppBusy: Boolean;
    procedure ExecuteCommands;
    procedure ExecuteParsedCommands(const CmdStr: string);
    function HookWndProc(var AMsg: TMessage): Boolean;
    procedure Notify(ACommands: TStrings);
  protected
    procedure BusyStateChanged; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Commands: TStrings read GetCommands;
  published
    property CorrectParams: Boolean read FCorrectParams write FCorrectParams default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property IgnoreAppBusy: Boolean read FIgnoreAppBusy write SetIgnoreAppBusy default False;
    property OnBusyChanged: TJvADCBusyEvent read FOnBusyChanged write FOnBusyChanged;
    property OnExecCommand: TJvADCMacroEvent read FOnExecCommand write FOnExecCommand;
    property OnExecParsedCmd: TJvADCParsedEvent read FOnExecParsedCmd write FOnExecParsedCmd;
  end;

implementation

uses
  SysUtils, Forms,
  JvResources, JvFinalize;

const
  sUnitName = 'JvDdeCmd';

const
  DdeTopicStr = 'System';

//=== { TAppDdeMgr } =========================================================

type
  TAppDdeMgr = class(TObject)
  private
    DdeServ: TDdeServerConv;
    Commands: TStringList;
    Components: TList;
    procedure DdeServerConvExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure NotifyComponents;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddComponent(AComponent: TComponent);
    procedure DeleteComponent(AComponent: TComponent);
  end;

var
  AppDdeMgr: TAppDdeMgr = nil;

constructor TAppDdeMgr.Create;
begin
  inherited Create;
  if Application.FindComponent(DdeTopicStr) = nil then
  begin
    DdeServ := TDdeServerConv.Create(Application);
    with DdeServ do
    begin
      Name := DdeTopicStr;
      OnExecuteMacro := DdeServerConvExecuteMacro;
    end;
    Commands := TStringList.Create;
    Components := TList.Create;
  end;
end;

destructor TAppDdeMgr.Destroy;
begin
  FreeAndNil(Components);
  FreeAndNil(Commands);
  inherited Destroy;
end;

procedure TAppDdeMgr.AddComponent(AComponent: TComponent);
begin
  Components.Add(AComponent);
end;

procedure TAppDdeMgr.DeleteComponent(AComponent: TComponent);
begin
  Components.Remove(AComponent);
end;

procedure TAppDdeMgr.DdeServerConvExecuteMacro(Sender: TObject; Msg: TStrings);
begin
  if Components <> nil then
  begin
    Commands.Add(Msg[0]);
    NotifyComponents;
  end;
end;

procedure TAppDdeMgr.NotifyComponents;
var
  I: Integer;
begin
  with Components do
    for I := 0 to Count - 1 do
      TJvAppDdeCmd(Items[I]).Notify(Commands);
  Commands.Clear;
end;

//=== { TJvAppDdeCmd } =======================================================

constructor TJvAppDdeCmd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCorrectParams := True;
  FEnabled := True;
  FIgnoreAppBusy := False;
  FModalState := False;
  FCommands := TStringList.Create;
  if not (csDesigning in ComponentState) then
  begin
    if not Assigned(AppDdeMgr) then
    begin
      AppDdeMgr := TAppDdeMgr.Create;
      AddFinalizeObjectNil(sUnitName, TObject(AppDdeMgr));
    end;
    AppDdeMgr.AddComponent(Self);
    Application.HookMainWindow(HookWndProc);
  end;
end;

destructor TJvAppDdeCmd.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    Application.UnhookMainWindow(HookWndProc);
    AppDdeMgr.DeleteComponent(Self);
  end;
  FreeAndNil(FCommands);
  inherited Destroy;
end;

function TJvAppDdeCmd.GetCommands: TStrings;
begin
  Result := FCommands;
end;

procedure TJvAppDdeCmd.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    BusyStateChanged;
  end;
end;

procedure TJvAppDdeCmd.SetIgnoreAppBusy(Value: Boolean);
begin
  if FIgnoreAppBusy <> Value then
  begin
    FIgnoreAppBusy := Value;
    BusyStateChanged;
  end;
end;

procedure TJvAppDdeCmd.SetModalState(Value: Boolean);
begin
  if FModalState <> Value then
  begin
    FModalState := Value;
    BusyStateChanged;
  end;
end;

function TJvAppDdeCmd.AppBusy: Boolean;
begin
  Result := (not FEnabled) or (FModalState and (not FIgnoreAppBusy));
end;

procedure TJvAppDdeCmd.BusyStateChanged;
begin
  if Assigned(FOnBusyChanged) then
    FOnBusyChanged(Self, AppBusy);
  ExecuteCommands;
end;

procedure TJvAppDdeCmd.ExecuteCommands;
begin
  with FCommands do
    while (not AppBusy) and (Count > 0) do
    begin
      try
        if Assigned(FOnExecCommand) then
          FOnExecCommand(Self, Strings[0]);
        if Assigned(FOnExecParsedCmd) then
          ExecuteParsedCommands(Strings[0]);
        Delete(0);
      except
        Delete(0);
        Application.HandleException(Self);
      end;
    end;
end;

procedure TJvAppDdeCmd.ExecuteParsedCommands(const CmdStr: string);
var
  I: Integer;
  CmdSPos, CmdEPos, ParamsSPos, ParamsEPos: Integer;
  StartCmd, StartParams, StartString: Boolean;
  S, Cmd: string;
  Params: TStringList;

  procedure CorrParams;
  var
    I, ErrCode, Value: Integer;
    C: ShortString;
  begin
    with Params do
      for I := 0 to Count - 1 do
      begin
        C := Strings[I];
        if Length(C) >= 2 then
        begin
          if C[1] = '"' then
            System.Delete(C, 1, 1);
          if C[Length(C)] = '"' then
            System.Delete(C, Length(C), 1);
          Strings[I] := C;
        end;
        Val(C, Value, ErrCode);
        if ErrCode = 0 then
          Objects[I] := Pointer(Value);
      end;
  end;

begin
  I := 1;
  StartCmd := False;
  StartParams := False;
  StartString := False;
  CmdSPos := 0;
  CmdEPos := 0;
  ParamsSPos := 0;
  // ParamsEPos := 0;
  Params := TStringList.Create;
  try
    S := Trim(CmdStr);
    while I <= Length(S) do
    begin
      if not StartCmd then
      begin
        if S[I] <> '[' then
          raise EJvADCParserError.CreateRes(@RsEErrorCommandStart);
        StartCmd := True;
        StartParams := False;
        StartString := False;
        CmdSPos := I + 1;
        CmdEPos := 0;
        ParamsSPos := 0;
        // ParamsEPos := 0;
        Params.Clear;
      end
      else
      begin
        if S[I] = '"' then
          StartString := not StartString
        else
        if not StartString then
        begin
          if (S[I] = ',') and StartParams then
          begin
            Params.Add(Copy(S, ParamsSPos, I - ParamsSPos));
            ParamsSPos := I + 1;
          end
          else
          if S[I] = '(' then
          begin
            CmdEPos := I - 1;
            StartParams := True;
            ParamsSPos := I + 1;
          end
          else
          if (S[I] = ')') and StartParams then
          begin
            ParamsEPos := I - 1;
            StartParams := False;
            Params.Add(Copy(S, ParamsSPos, ParamsEPos - ParamsSPos + 1));
          end
          else
          if (S[I] = ']') and (not StartParams) then
          begin
            if CmdEPos = 0 then
              CmdEPos := I - 1;
            Cmd := AnsiUpperCase(Copy(S, CmdSPos, CmdEPos - CmdSPos + 1));
            if FCorrectParams then
              CorrParams;
            FOnExecParsedCmd(Self, Cmd, Params);
            StartCmd := False;
          end;
        end;
      end;
      Inc(I);
    end;
    if StartCmd or StartParams or StartString then
      raise EJvADCParserError.CreateResFmt(@RsEErrorCommandFormat, [S]);
  finally
    Params.Free;
  end;
end;

function TJvAppDdeCmd.HookWndProc(var AMsg: TMessage): Boolean;
begin
  Result := False;
  if AMsg.Msg = WM_ENABLE then
    SetModalState(not TWMEnable(AMsg).Enabled);
end;

procedure TJvAppDdeCmd.Notify(ACommands: TStrings);
begin
  FCommands.AddStrings(ACommands);
  ExecuteCommands;
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.
