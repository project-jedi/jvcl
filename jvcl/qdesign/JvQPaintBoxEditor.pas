{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

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

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQPaintBoxEditor;

interface
uses
  SysUtils, Classes,
  
  
  QForms, QGraphics, QImgList, QDialogs, QControls, Types,
  {$IFDEF LINUX}
  ClxEditors,
  {$ENDIF LINUX}
  
  
  
  DesignIntf, DesignEditors, DesignMenus;
  

type
  TJvPaintBoxEditor = class(TDefaultEditor)
  public
    
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    
  end;

implementation


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


end.
 
