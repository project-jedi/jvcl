{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgShadowEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgShadowEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, comctrls
  {$IFDEF COMPILER6_UP}, DesignIntf, DesignWindows, DesignEditors{$ELSE}{$IFDEF COMPILER4_UP}, dsgnintf{$ENDIF}{$ENDIF};

type

  TJvgShadowEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation
uses StdCtrls, JvgShadow;

{ TJvgShadowEditor }

procedure TJvgShadowEditor.ExecuteVerb(Index: Integer);
var
  sl: TStringList;
  i: integer;
  sNewName: string;
  Shadow: TJvgShadow;

  procedure UpdateIt(Edit: TCustomEdit);
  var
    Sh: TJvgShadow;
    i, NewNameIndex: integer;
    fExists: boolean;
  begin
    if sl.IndexOf(Edit.Name) <> -1 then exit;

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
      fExists := false;
      inc(NewNameIndex);
      sNewName := 'JvgShadow' + IntToStr(NewNameIndex);
      for i := 0 to (Shadow.Owner as TForm).ControlCount - 1 do
      begin
        fExists := CompareText((Shadow.Owner as TForm).Controls[i].Name, sNewName) = 0;
        if fExists then break;
      end;
    until not fExists;

    Sh.Name := sNewName;
  end;

begin
  case Index of
    0:
      begin
        sl := TStringList.Create;
        Shadow := Component as TJvgShadow;
        for i := 0 to Shadow.Parent.ControlCount - 1 do
          if (Shadow.Parent.Controls[i] is TJvgShadow) and ((Shadow.Parent.Controls[i] as TJvgShadow).Control <> nil) then
            sl.Add((Shadow.Parent.Controls[i] as TJvgShadow).Control.Name);

        for i := 0 to Shadow.Parent.ControlCount - 1 do
          if Shadow.Parent.Controls[i] is TCustomEdit then
          begin
            UpdateIt(Shadow.Parent.Controls[i] as TCustomEdit);
          end;
        sl.Free;
      end;
  end;
end;

function TJvgShadowEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Update all edit control';
  end;
end;

function TJvgShadowEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
