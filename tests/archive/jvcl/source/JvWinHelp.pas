{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWinHelp.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvWinHelp;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, JvTypes, JvComponent;

type
  TJvWinHelp = class(TJvComponent)
  private
    FHelpFile: string;
    FOwner: TComponent;
    function GetHelpFile: PChar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HelpFile: string read FHelpFile write FHelpFile;
    function ShowContextualHelp(Control: TWinControl): Boolean;
    function ExecuteCommand(MacroCommand: string): Boolean;
    function ShowHelp(Control: TWinControl): Boolean;
    function ShowContents: Boolean;
    function ShowHelpOnHelp: Boolean;
    function ShowIndex: Boolean;
    function ShowKeyword(Keyword: string): Boolean;
    function ShowPartialKeyWord(Keyword: string): Boolean;
    function SetWindowPos(Left, Top, Width, Height: Integer; Visibility: Integer): Boolean;
  end;

implementation

resourcestring
  RC_OwnerForm = 'Owner must be of type TForm';

  {**************************************************}

constructor TJvWinHelp.Create(AOwner: TComponent);
begin
  inherited;
  FHelpFile := '';
  FOwner := AOwner;

  while FOwner.GetParentComponent <> nil do
    FOwner := FOwner.GetParentComponent;
  if not (FOwner is TForm) then
    raise EJVCLException.Create(RC_OwnerForm);
end;

{**************************************************}

destructor TJvWinHelp.Destroy;
begin
  WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_QUIT, 0);
  inherited;
end;

{**************************************************}

function TJvWinHelp.ExecuteCommand(MacroCommand: string): Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_COMMAND, Longint(PChar(MacroCommand)));
end;

{**************************************************}

function TJvWinHelp.GetHelpFile: PChar;
begin
  if FHelpFile = '' then
    Result := PChar((FOwner as TForm).HelpFile)
  else
    Result := PChar(FHelpFile);
end;

{**************************************************}

function TJvWinHelp.SetWindowPos(Left, Top, Width, Height,
  Visibility: Integer): Boolean;
var
  HelpInfo: HELPWININFO;
begin
  HelpInfo.wStructSize := SizeOf(HELPWININFO);
  HelpInfo.x := Left;
  HelpInfo.y := Top;
  HelpInfo.dx := Width;
  HelpInfo.dy := Height;
  HelpInfo.wMax := Visibility;

  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_SETWINPOS, Longint(@HelpInfo));
end;

{**************************************************}

function TJvWinHelp.ShowContents: Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_CONTENTS, 0);
end;

{**************************************************}

function TJvWinHelp.ShowContextualHelp(Control: TWinControl): Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_CONTEXTPOPUP, Control.HelpContext);
end;

{**************************************************}

function TJvWinHelp.ShowHelp(Control: TWinControl): Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_CONTEXT, Control.HelpContext);
end;

{**************************************************}

function TJvWinHelp.ShowHelpOnHelp: Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_HELPONHELP, 0);
end;

{**************************************************}

function TJvWinHelp.ShowIndex: Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_INDEX, 0);
end;

{**************************************************}

function TJvWinHelp.ShowKeyword(Keyword: string): Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_KEY, Longint(PChar(KeyWord)));
end;

{**************************************************}

function TJvWinHelp.ShowPartialKeyWord(Keyword: string): Boolean;
begin
  Result := WinHelp(TWinControl(FOwner).Handle, GetHelpFile, HELP_PARTIALKEY, Longint(PChar(KeyWord)));
end;

end.
