{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QDesignWindows.pas, released on 2004-05-14

The Initial Developer of the Original Code is Andreas Hausladen
                                              [Andreas dott Hausladen att gmx dott de]
Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

Known Issues:
----------------------------------------------------------------------------}
// $Id$

unit QDesignWindows;

interface

uses
  Classes, ClxDesignWindows;

type
  TDesignWindow = class(TClxDesignWindow)
  protected
    function UniqueName(Component: TComponent): string; override;
  end;

implementation

{ TDesignWindow }

function TDesignWindow.UniqueName(Component: TComponent): string;
begin
  Result := Designer.UniqueName(Component.ClassName);
end;

end.
