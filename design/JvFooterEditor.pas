{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAnimatedEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}

unit JvFooterEditor;

interface
uses
  Windows, Forms, Graphics, ImgList,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  SysUtils, Classes, Dialogs, Controls;

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

//=== TJvFooterEditor ========================================================

function TJvFooterEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;

function TJvFooterEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := SAddButtonText;
    1:
      Result := '-'; // do not localize
    2:
      Result := SMSOffice;
    3:
      Result := SMSEnterpriseManagerWizard;
    4:
      Result := SDialogMode;
  end;
end;

procedure TJvFooterEditor.ExecuteVerb(Index: Integer);
const
  cButtonHeight = 50;
var
  FButton: TJvFooterBtn;
begin
  case Index of
    0:
      Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, 50);
    1:
      ;
    2:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        FButton.Caption := SHelpButton;
        FButton.Alignment := taLeftJustify;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        FButton.Caption := SOKButton;
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        FButton.Caption := SCancelButton;
        FButton.Cancel := True;
      end;
    3:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        FButton.Caption := SPrevious;
        FButton.SpaceInterval := 0;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        FButton.Caption := SNext;
        FButton.Default := True;
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        FButton.Caption := SCloseButton;
      end;
    4:
      begin
        FButton := TJvFooterBtn(Designer.CreateComponent(TJvFooterBtn, Component, 0, 0, 0, cButtonHeight));
        FButton.Caption := SOKButton;
        FButton.SpaceInterval := 0;
        FButton.Alignment := taCenter;
      end;
  end;
end;

procedure TJvFooterEditor.Edit;
begin
  // We don't need to add band on double click
end;

end.
