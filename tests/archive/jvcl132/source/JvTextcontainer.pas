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

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

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
Unit JvTextcontainer;

Interface

Uses
  SysUtils, Classes ,JvComponent;

Type
  TJvTextContainer = Class(TJvComponent)
  Private
    { Private declarations }
    FLines: TStrings;

    Procedure SetLines( aList: TStrings );
    Function GetText: String;

  Public
    { Public declarations }
    Constructor Create( aOwner: TComponent ); override;
    Destructor Destroy; override;

    Property Text: String read GetText;
  Published
    { Published declarations }
    Property Lines: TStrings read FLines write SetLines;

  End;


Implementation


{+---------------------------
 | Methods of TJvTextContainer 
 +--------------------------}
Procedure TJvTextContainer.SetLines( aList: TStrings );
  Begin
    FLines.Assign( aList );
  End; { TJvTextContainer.SetLines }

Function TJvTextContainer.GetText: String;
  Begin
    Result := FLines.Text;
  End; { TJvTextContainer.GetText }

Constructor TJvTextContainer.Create( aOwner: TComponent ); 
  Begin
    inherited Create( aOwner );
    FLines := TStringlist.create;
  End; { TJvTextContainer.Create }

Destructor TJvTextContainer.Destroy; 
  Begin
    FLines.Free;
    inherited Destroy;
  End; { TJvTextContainer.Destroy }

End.
