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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvWinHelp;

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Controls, Forms, Menus,
  {$IFDEF VisualCLX}
  Qt,
  {$ENDIF VisualCLX}
  JvTypes, JvComponent;

type
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
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_COMMAND, Longint(PChar(MacroCommand)));
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
    {$IFDEF VCL}
    Result := TWinControl(FOwner).Handle
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Result := QWidget_winId(TWidgetControl(FOwner).Handle)
    {$ENDIF VisualCLX}
  else
  if Application <> nil then
  begin
    if (Screen <> nil) and (Screen.ActiveForm <> nil) then
      {$IFDEF VCL}
      Result := Screen.ActiveForm.Handle
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      Result := QWidget_winId(Screen.ActiveForm.Handle)
      {$ENDIF VisualCLX}
    else
    if Application.MainForm <> nil then
      {$IFDEF VCL}
      Result := Application.MainForm.Handle
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      Result := QWidget_winId(Application.MainForm.Handle)
      {$ENDIF VisualCLX}
    {$IFDEF VCL}
    else
    if not (csDestroying in Application.ComponentState) then
      Result := Application.Handle;
    {$ENDIF VCL}
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
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_SETWINPOS, Longint(@HelpInfo));
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
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_KEY, Longint(PChar(Keyword)));
end;

function TJvWinHelp.ShowPartialKeyWord(Keyword: string): Boolean;
begin
  Result := WinHelp(GetOwnerHandle, GetHelpFile, HELP_PARTIALKEY, Longint(PChar(Keyword)));
end;

end.

