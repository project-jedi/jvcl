{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgShadowEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgShadowEditor;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignWindows, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  TJvgShadowEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  StdCtrls,
  JvgShadow, JvDsgnConsts;

procedure TJvgShadowEditor.ExecuteVerb(Index: Integer);
var
  Sl: TStringList;
  I: Integer;
  NewName: string;
  Shadow: TJvgShadow;

  procedure UpdateIt(Edit: TCustomEdit);
  var
    Sh: TJvgShadow;
    I, NewNameIndex: Integer;
    Exists: Boolean;
  begin
    if Sl.IndexOf(Edit.Name) <> -1 then
      Exit;

    TEdit(Edit).BorderStyle := bsNone;
    Sh := TJvgShadow.Create(Component.Owner);
    Sh.Parent := Shadow.Parent;
    Sh.SetBounds(Edit.Left, Edit.Top, Edit.Width - 2, Edit.Height - 2);
    Sh.Shadowed := Shadow.Shadowed;
    Sh.ShadowDepth := Shadow.ShadowDepth;
    Sh.Style.Inner := Shadow.Style.Inner;
    Sh.Style.Outer := Shadow.Style.Outer;
    Sh.Control := Edit;

    NewNameIndex := 0;
    repeat
      Exists := False;
      Inc(NewNameIndex);
      NewName := 'JvgShadow' + IntToStr(NewNameIndex);
      for I := 0 to (Shadow.Owner as TForm).ControlCount - 1 do
      begin
        Exists := CompareText((Shadow.Owner as TForm).Controls[I].Name, NewName) = 0;
        if Exists then
          Break;
      end;
    until not Exists;

    Sh.Name := NewName;
  end;

begin
  case Index of
    0:
      begin
        Sl := TStringList.Create;
        Shadow := Component as TJvgShadow;
        for I := 0 to Shadow.Parent.ControlCount - 1 do
          if (Shadow.Parent.Controls[I] is TJvgShadow) and ((Shadow.Parent.Controls[I] as TJvgShadow).Control <> nil) then
            Sl.Add((Shadow.Parent.Controls[I] as TJvgShadow).Control.Name);

        for I := 0 to Shadow.Parent.ControlCount - 1 do
          if Shadow.Parent.Controls[I] is TCustomEdit then
            UpdateIt(Shadow.Parent.Controls[I] as TCustomEdit);
        Sl.Free;
      end;
  end;
end;

function TJvgShadowEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsUpdateAllEditControl;
  end;
end;

function TJvgShadowEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
