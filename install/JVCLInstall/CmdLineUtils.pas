{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: CmdLineUtils.pas, released on 2004-04-09.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit CmdLineUtils;

interface

uses
  Windows, SysUtils, Contnrs, Utils;

type
  TCmdOptions = class(TObject)
  private
    FIgnoreIDE: Boolean;
    FJclPath: string;
    FKeepFiles: Boolean;
    FIgnoreBCB: Boolean;
    FIgnoreDelphi: Boolean;
    FHelp: Boolean;
    FLang: string;
    FAutoUpdate: Boolean;
    FRegistryKeyDelphi: string;
    FRegistryKeyBCB: string;
    FRegistryKeyBDS: string;

    FItemList: TObjectList;
    procedure ShowHelp;
  protected
    procedure AddBool(const Name, Help: string; var Value: Boolean);
    procedure AddString(const Name, Help: string; var Value: string);
    procedure AddDir(const Name, Help: string; var Value: string);
    procedure AddSpace;

    procedure Init; virtual;
  public
    constructor Create;

    property IgnoreIDE: Boolean read FIgnoreIDE write FIgnoreIDE;
    property IgnoreBCB: Boolean read FIgnoreBCB write FIgnoreBCB;
    property IgnoreDelphi: Boolean read FIgnoreDelphi write FIgnoreDelphi;
    property JclPath: string read FJclPath write FJclPath;
    property KeepFiles: Boolean read FKeepFiles write FKeepFiles;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate;
    property RegistryKeyDelphi: string read FRegistryKeyDelphi write FRegistryKeyDelphi;
    property RegistryKeyBCB: string read FRegistryKeyBCB write FRegistryKeyBCB;
    property RegistryKeyBDS: string read FRegistryKeyBDS write FRegistryKeyBDS;

    property Help: Boolean read FHelp;
    property Lang: string read FLang;
  end;

var
  CmdOptions: TCmdOptions;

implementation

procedure TCmdOptions.Init;
begin
  AddBool('--ignore-ide', 'Start installer even if Delphi/BCB is running.', FIgnoreIDE);
  AddBool('--ignore-bcb', 'Do not display and compile BCB versions.', FIgnoreBCB);
  AddBool('--ignore-delphi', 'Do not display and compile Delphi versions.', FIgnoreDelphi);
  AddSpace;
  AddDir('--jcl-path=', 'Set the JCL root directory to X.', FJclPath);
  AddBool('--keep-files', 'Do not call "clean".', FKeepFiles);
  AddBool('--autoupdate', 'Updates all IDEs where JVCL 3 is installed.', FAutoUpdate);
  AddSpace;
  AddString('-rDelphi=', 'Sets the Registry path for the Delphi IDEs.', FRegistryKeyDelphi);
  AddString('-rBCB=', 'Sets the Registry path for the BCB IDEs.', FRegistryKeyBCB);
  AddString('-rBDS=', 'Sets the Registry path for the BDS IDEs. (-rBDS=Win32Only)', FRegistryKeyBDS);
  AddString('--lang=', 'Sets the Installer''s language to X.', FLang);
  AddBool('--help', 'Show this screen.', FHelp);
  AddBool('/?', '', FHelp);
  AddBool('-h', '', FHelp);
end;

type
  TItem = class(TObject)
  private
    FName: string;
    FHelp: string;
  public
    constructor Create(const AName, AHelp: string);
    function IsItem(const ParStr: string): Boolean; virtual;

    property Name: string read FName;
    property Help: string read FHelp;
  end;

  TStringItem = class(TItem)
  private
    FValue: ^string;
  public
    constructor Create(const AName, AHelp: string; var Value: string);
    function IsItem(const ParStr: string): Boolean; override;
  end;

  TBoolItem = class(TItem)
  private
    FValue: ^Boolean;
  public
    constructor Create(const AName, AHelp: string; var Value: Boolean);
    function IsItem(const ParStr: string): Boolean; override;
  end;

  TDirItem = class(TStringItem)
  private
    function IsItem(const ParStr: string): Boolean; override;
  end;


{ TCmdOptions }

procedure TCmdOptions.AddBool(const Name, Help: string;
  var Value: Boolean);
begin
  FItemList.Add(TBoolItem.Create(Name, Help, Value));
end;

procedure TCmdOptions.AddDir(const Name, Help: string; var Value: string);
begin
  FItemList.Add(TDirItem.Create(Name, Help, Value));
end;

procedure TCmdOptions.AddSpace;
begin
  FItemList.Add(TItem.Create('', ''));
end;

procedure TCmdOptions.AddString(const Name, Help: string;
  var Value: string);
begin
  FItemList.Add(TStringItem.Create(Name, Help, Value));
end;

constructor TCmdOptions.Create;
var
  i, Index: Integer;
  S: string;
begin
  FItemList := TObjectList.Create;
  try
    Init;
    for i := 1 to ParamCount do
    begin
      S := ParamStr(I);
      for Index := 0 to FItemList.Count - 1 do
      begin
        if TItem(FItemList[Index]).IsItem(S) then
          Break;
      end;
    end;
    if Help then
    begin
      {$IFDEF MSWINDOWS}
      AllocConsole;
      {$ENDIF MSWINDOWS}
      ShowHelp;
      Halt(1);
    end;
  finally
    FItemList.Free;
  end;
end;

function PadString(const S: string; Count: Integer): string;
var
  Len: Integer;
begin
  Len := Length(S);
  if Len < Count then
  begin
    SetLength(Result, Count);
    Move(S[1], Result[1], Count);
    FillChar(Result[Len + 1], Count - Len, ' ');
  end
  else
    Result := S;
end;

procedure TCmdOptions.ShowHelp;
var
  i: Integer;
  Item: TItem;
  S: string;
begin
  WriteLn;
  for i := 0 to FItemList.Count - 1 do
  begin
    Item := TItem(FItemList[i]);
    if Item.Name = '' then
      WriteLn;
    if Item.Help <> '' then
    begin
      S := Item.Name;
      if Item is TStringItem then
        S := S + 'X';
      WriteLn(PadString(S, 20), '  ', Item.Help);
    end;
  end;
  WriteLn;
  ReadLn;
end;

{ TItem }

constructor TItem.Create(const AName, AHelp: string);
begin
  inherited Create;
  FName := AName;
  FHelp := AHelp;
end;

function TItem.IsItem(const ParStr: string): Boolean;
begin
  Result := False;
end;

{ TStringItem }

constructor TStringItem.Create(const AName, AHelp: string;
  var Value: string);
begin
  inherited Create(AName, AHelp);
  FValue := @Value;
end;

function TStringItem.IsItem(const ParStr: string): Boolean;
begin
  Result := StartsWith(ParStr, Name, True);
  if Result then
    FValue^ := Copy(ParStr, Length(Name) + 1, MaxInt);
end;

{ TBoolItem }

constructor TBoolItem.Create(const AName, AHelp: string;
  var Value: Boolean);
begin
  inherited Create(AName, AHelp);
  FValue := @Value;
end;

function TBoolItem.IsItem(const ParStr: string): Boolean;
begin
  Result := CompareText(ParStr, Name) = 0;
  if Result then
    FValue^ := True;
end;

{ TDirItem }

function TDirItem.IsItem(const ParStr: string): Boolean;
var
  OldDir: string;
begin
  OldDir := FValue^;
  Result := inherited IsItem(ParStr);
  if Result then
    if not DirectoryExists(FValue^) then
      FValue^ := OldDir;
end;

initialization
  CmdOptions := TCmdOptions.Create;

finalization
  CmdOptions.Free;

end.
