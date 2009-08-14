{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFooterEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFooterEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Forms, Graphics, ImgList, Dialogs, Controls,
  VCLEditors, DesignIntf, DesignEditors, DesignMenus;

type
  TJvFooterEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

implementation

uses
  Consts,
  JvTypes, JvFooter, JvDsgnConsts;

//=== { TJvFooterEditor } ====================================================

function TJvFooterEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

function TJvFooterEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsAddButtonText;
    1:
      Result := '-'; // do not localize
    2:
      Result := RsMSOffice;
    3:
      Result := RsMSEnterpriseManagerWizard;
    4:
      Result := RsDialogMode;
  end;
end;

procedure TJvFooterEditor.ExecuteVerb(Index: Integer);
const
  cButtonHeight = 50;
var
  Button: TJvFooterBtn;
begin
  case Index of
    0:
      Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight);
    1:
      ;
    2:
      begin
        Button := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        Button.Caption := SHelpButton;
        Button.Alignment := taLeftJustify;
        Button := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        Button.Caption := SOKButton;
        Button.Default := True;
        Button := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        Button.Caption := SCancelButton;
        Button.Cancel := True;
      end;
    3:
      begin
        Button := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        Button.Caption := RsPrevious;
        Button.SpaceInterval := 0;
        Button := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        Button.Caption := RsNext;
        Button.Default := True;
        Button := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        Button.Caption := SCloseButton;
      end;
    4:
      begin
        Button := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        Button.Caption := SOKButton;
        Button.SpaceInterval := 0;
        Button.Alignment := taCenter;
      end;
  end;
end;

procedure TJvFooterEditor.Edit;
begin
  // We don't need to add band on double click
end;

end.