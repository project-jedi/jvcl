{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMLButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TJvMultilineButton = class(TButton)
  private
    FMultiline: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetMultiline(const Value: Boolean);
  public
    procedure CreateParams(var params: TCreateParams); override;
    constructor Create(aOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Multiline: Boolean read FMultiline write SetMultiline default True;
    property Caption: string read GetCaption write SetCaption;
  end;

implementation

{ TJvMultilineButton }

constructor TJvMultilineButton.Create(aOwner: TComponent);
begin
  inherited;
  FMultiline := True;
end;

procedure TJvMultilineButton.CreateParams(var params: TCreateParams);
begin
  inherited;
  if FMultiline then
    params.Style := params.Style or BS_MULTILINE;
end;

function TJvMultilineButton.GetCaption: string;
begin
  Result := Stringreplace(inherited Caption, #13, '|', [rfReplaceAll]);
end;

procedure TJvMultilineButton.SetCaption(const Value: string);
begin
  if value <> Caption then
  begin
    inherited Caption := Stringreplace(value, '|', #13, [rfReplaceAll]);
    Invalidate;
  end;
end;

procedure TJvMultilineButton.SetMultiline(const Value: Boolean);
begin
  if FMultiline <> Value then
  begin
    FMultiline := Value;
    RecreateWnd;
  end;
end;

end.
