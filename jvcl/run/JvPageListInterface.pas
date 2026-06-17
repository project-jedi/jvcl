{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListInterface.pas, released on 2019-04-25.

The Initial Developer of the Original Code is Markus Humm <Markus dott Humm att gmail dott com>
Portions created by Markus Humm are Copyright (C) 2019 Markus Humm
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPageListInterface;

interface

type
  IPageList = interface
    ['{6BB90183-CFB1-4431-9CFD-E9A032E0C94C}']
    function CanChange(AIndex: Integer): Boolean;
    procedure SetActivePageIndex(AIndex: Integer);
    function GetPageCount: Integer;
    function GetPageCaption(AIndex: Integer): string;
    procedure AddPage(const ACaption: string);
    procedure DeletePage(Index: Integer);
    procedure MovePage(CurIndex, NewIndex: Integer);
    procedure PageCaptionChanged(Index: Integer; const NewCaption: string);
  end;

implementation

end.
