{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

{+------------------------------------------------------------
 | Unit Textcontainer
 |
 | Version: 1.0  Created: 12.03.99
 |               Last Modified: 12.03.99
 | Author : P. Below
 | Project: Common components
 | Description:
 |   Implements a simple component to import textfiles into
 |   a project at design-time.
 +------------------------------------------------------------}
unit JvTextcontainer;

interface

uses
  SysUtils, Classes, JvComponent;

type
  TJvTextContainer = class(TJvComponent)
  private
    { Private declarations }
    FLines: TStrings;

    procedure SetLines(aList: TStrings);
    function GetText: string;

  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    property Text: string read GetText;
  published
    { Published declarations }
    property Lines: TStrings read FLines write SetLines;

  end;

implementation

{+---------------------------
 | Methods of TJvTextContainer
 +--------------------------}

procedure TJvTextContainer.SetLines(aList: TStrings);
begin
  FLines.Assign(aList);
end; { TJvTextContainer.SetLines }

function TJvTextContainer.GetText: string;
begin
  Result := FLines.Text;
end; { TJvTextContainer.GetText }

constructor TJvTextContainer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLines := TStringlist.create;
end; { TJvTextContainer.Create }

destructor TJvTextContainer.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end; { TJvTextContainer.Destroy }

end.
