{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNavPaneEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvNavPaneEditors;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Controls, Forms, ToolWin, Menus, ActnList, ComCtrls, ImgList,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QControls, QForms, QToolWin, QMenus, QActnList, QComCtrls, QImgList,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus, DesignWindows,
  {$ELSE}
  DsgnIntf, DsgnWnds,
  {$ENDIF COMPILER6_UP}
  JvDsgnEditors, JvNavigationPane;

type
  TJvNavPanePageImageIndexProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

  TJvNavPanelHeaderImageIndexProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

  TJvNavPanelButtonImageIndexProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

  TJvNavIconButtonImageIndexProperty = class(TJvDefaultImageIndexProperty)
  protected
    function ImageList: TCustomImageList; override;
  end;

implementation

uses
  JvPageList;

//=== TJvNavPanePageImageIndexProperty =======================================

function TJvNavPanePageImageIndexProperty.ImageList: TCustomImageList;
var
  P: TJvNavigationPane;
begin
  P := TJvNavigationPane(TJvNavPanelPage(GetComponent(0)).PageList);
  if P = nil then
    Result := nil
  else
  if P.SmallImages <> nil then // small images fi better into the OI, so prefer those
    Result := P.SmallImages
  else
    Result := P.LargeImages;
end;

//=== TJvNavPanelHeaderImageIndexProperty ====================================

function TJvNavPanelHeaderImageIndexProperty.ImageList: TCustomImageList;
begin
  Result := TJvNavPanelHeader(GetComponent(0)).Images;
end;

//=== TJvNavPanelButtonImageIndexProperty ====================================

function TJvNavPanelButtonImageIndexProperty.ImageList: TCustomImageList;
begin
  Result := TJvNavPanelButton(GetComponent(0)).Images;
end;

//=== TJvNavIconButtonImageIndexProperty =====================================

function TJvNavIconButtonImageIndexProperty.ImageList: TCustomImageList;
begin
  Result := TJvNavIconButton(GetComponent(0)).Images;
end;

end.
