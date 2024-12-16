{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWinHelp.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvWinHelp;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Controls, Forms, Menus,
  JvTypes, JvComponentBase;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvWinHelp = class(TJvComponent)
  private
    FHelpFile: string;
    FOwner: TComponent;
    function GetHelpFile: PChar;
  protected
    function GetOwnerHandle: THandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ShowContextualHelp(Control: TWinControl): Boolean;
    function ExecuteCommand(MacroCommand: string): Boolean;
    function ShowHelp(Control: TWinControl): Boolean;
    function ShowContents: Boolean;
    function ShowHelpOnHelp: Boolean;
    function ShowIndex: Boolean;
    function ShowKeyword(Keyword: string): Boolean;
    function ShowPartialKeyWord(Keyword: string): Boolean;
    function SetWindowPos(Left, Top, Width, Height: Integer; Visibility: Integer): Boolean;
  published
    property HelpFile: string read FHelpFile write FHelpFile;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JvResources;

constructor TJvWinHelp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHelpFile := '';
  FOwner := AOwner;
  while FOwner.GetParentComponent <> nil do
    FOwner := FOwner.GetParentComponent;
  if not (FOwner is TCustomForm) then
    raise EJVCLException.CreateRes(@RsEOwnerForm);
end;

destructor TJvWinHelp.Destroy;
begin
  WinHelp(GetOwnerHandle, GetHelpFile, HELP_QUIT, 0);
  inherited Destroy;
end;

function TJvWinHelp.ExecuteCommand(MacroCommand: string): Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_COMMAND, LPARAM(PChar(MacroCommand)));
end;

function TJvWinHelp.GetHelpFile: PChar;
begin
  if (FHelpFile = '') and (FOwner is TForm) and not (csDestroying in TForm(FOwner).ComponentState) then
    Result := PChar(TForm(FOwner).HelpFile)
  else
    Result := PChar(FHelpFile);
end;

function TJvWinHelp.GetOwnerHandle: THandle;
begin
  Result := 0;
  if (FOwner is TWinControl) and not (csDestroying in TWinControl(FOwner).ComponentState) then
    Result := TWinControl(FOwner).Handle
  else
  if Application <> nil then
  begin
    if (Screen <> nil) and (Screen.ActiveForm <> nil) then
      Result := Screen.ActiveForm.Handle
    else
    if Application.MainForm <> nil then
      Result := Application.MainForm.Handle
    else
    if not (csDestroying in Application.ComponentState) then
      Result := Application.Handle;
  end;
end;

function TJvWinHelp.SetWindowPos(Left, Top, Width, Height, Visibility: Integer): Boolean;
var
  HelpInfo: HELPWININFO;
begin
  HelpInfo.wStructSize := SizeOf(HELPWININFO);
  HelpInfo.x := Left;
  HelpInfo.y := Top;
  HelpInfo.dx := Width;
  HelpInfo.dy := Height;
  HelpInfo.wMax := Visibility;
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_SETWINPOS, LPARAM(@HelpInfo));
end;

function TJvWinHelp.ShowContents: Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_CONTENTS, 0);
end;

function TJvWinHelp.ShowContextualHelp(Control: TWinControl): Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_CONTEXTPOPUP, Control.HelpContext);
end;

function TJvWinHelp.ShowHelp(Control: TWinControl): Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_CONTEXT, Control.HelpContext);
end;

function TJvWinHelp.ShowHelpOnHelp: Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_HELPONHELP, 0);
end;

function TJvWinHelp.ShowIndex: Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_INDEX, 0);
end;

function TJvWinHelp.ShowKeyword(Keyword: string): Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_KEY, LPARAM(PChar(Keyword)));
end;

function TJvWinHelp.ShowPartialKeyWord(Keyword: string): Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_PARTIALKEY, LPARAM(PChar(Keyword)));
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
