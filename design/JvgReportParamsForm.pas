{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReportParamsForm.PAS, released on 2003-01-15.

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

{$I jvcl.inc}

unit JvgReportParamsForm;

interface

uses
  Windows, Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids,
  StdCtrls, Buttons, ExtCtrls, Mask,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvgStringGrid, JvgReport, JvComponent;

type
  TJvgRepParamsEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TJvgReportParamsForm = class(TJvForm)
    SB: TScrollBox;
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
  private
  public
  end;

implementation

uses
  JvgReportParamsEditor, JvDsgnConsts;

{$R *.dfm}

procedure TJvgRepParamsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      TJvgReportParamsEditor(Component).Edit;
  end;
end;

function TJvgRepParamsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsEditParamsEllipsis;
  end;
end;

function TJvgRepParamsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


end.
