{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPaintBoxEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPaintBoxEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Windows, Forms, Graphics, ImgList, Dialogs, Controls,
  {$IFDEF COMPILER6_UP}
  VCLEditors, DesignIntf, DesignEditors, DesignMenus;
  {$ELSE}
  DsgnIntf;
  {$ENDIF COMPILER6_UP}

type
  TJvPaintBoxEditor = class(TDefaultEditor)
  public
    {$IFDEF COMPILER6_UP}
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF COMPILER6_UP}
  end;

implementation

{$IFDEF COMPILER6_UP}
procedure TJvPaintBoxEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
begin
  if CompareText(PropertyEditor.GetName, 'OnPaint') = 0 then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end
  else
    inherited EditProperty(PropertyEditor, Continue);
end;
{$ELSE}
procedure TJvPaintBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
begin
  if CompareText(PropertyEditor.GetName, 'OnPaint') = 0 then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end
  else
    inherited EditProperty(PropertyEditor, Continue, FreeEditor);
end;
{$ENDIF COMPILER6_UP}

end.
 
